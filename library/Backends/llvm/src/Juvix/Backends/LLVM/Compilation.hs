module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import qualified Juvix.Backends.LLVM.Codegen.Closure as Closure
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as LLVM (Definition, Operand (..))
import qualified LLVM.AST.Constant as LLVM (Constant (..))
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Type as LLVM
import LLVM.IRBuilder.Module
import qualified LLVM.Pretty as LLVM
import qualified Prelude as P
import qualified Juvix.Backends.LLVM.Pass.Types as Types
import qualified Juvix.Backends.LLVM.Pass.ClosureConversion as ClosureConversion

--------------------------------------------------------------------------------
-- Top Level Compilation
--------------------------------------------------------------------------------

preProcess ::
  ErasedAnn.AnnTerm PrimTy RawPrimVal -> Types.Annotated Types.TermClosure
preProcess = ClosureConversion.op

-- | Compile the input program to an LLVM module.
-- TODO: maybe do something smarter with the module name?
compileProgram ::
  Monad m =>
  -- | Term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  Feedback.FeedbackT [] P.String m Text
compileProgram t =
  case Block.runEnvState (register t) mempty of
    (Right _, state) ->
      state
        |> Types.moduleAST
        |> LLVM.ppllvm
        |> toStrict
        |> return
    (Left err, _) -> do
      show err
        |> Feedback.fail

register ::
  Types.Define m => ErasedAnn.AnnTerm PrimTy RawPrimVal -> m LLVM.Operand
register t = do
  Closure.register
  Block.defineMalloc
  Block.defineFree
  mkMain (preProcess t)

--------------------------------------------------------------------------------
-- Function Declaration
--------------------------------------------------------------------------------

-- | Write the main function of the module. Here two distinct cases can be
-- observed:
--
-- * The term @t@ is a lambda abstraction; this is the case when the main
-- function takes one or more arguments. Within the scope of the input
-- program, this lambda is never applied, as this only happens when the program
-- is executed.
-- A global function is written for the lambda and called in the main function.
-- The arguments for the call are passed on from main.
--
-- * The term @t@ is any other term than a lambda abstraction, we can write the
-- body of the main function by compiling @t@. The main function itself does
-- not have any parameters in this case.
mkMain ::
  Types.Define m =>
  -- | Term to compile.
  Types.Annotated Types.TermClosure ->
  m LLVM.Operand
mkMain t@(Types.Ann _usage ty _t') = do
  let (paramTys, returnTy) = functionTypeLLVM ty
      paramNames =
        zipWith
          (\l r -> Block.internName (l <> r))
          (replicate (length paramTys) "arg")
          (fmap (intern . show) [1 ..])
      params = zip paramTys paramNames
  Block.defineFunction returnTy "main" params $
    do
      case params of
        _ : _ -> do
          lamBody <- compileTerm t
          arguments <- traverse Block.externf paramNames
          called <- Block.call returnTy lamBody (zip arguments (repeat []))
          Block.ret called
        [] -> do
          res <- compileTerm t
          Block.ret res

--------------------------------------------------------------------------------
-- Term Handling
--------------------------------------------------------------------------------

-- | Compile a term to its equivalent LLVM code.
compileTerm ::
  Types.Define m => Types.Annotated Types.TermClosure -> m LLVM.Operand
compileTerm (Types.Ann _usage ty t) =
  case t of
    Types.LamM as body -> compileLam ty as body
    Types.AppM fn arguments -> compileApp ty fn arguments
    Types.Var sym -> compileVar sym
    Types.Prim _t -> mkPrim t ty

compileVar ::
  (HasState "symTab" Types.SymbolTable m, HasThrow "err" Types.Errors m) =>
  NameSymbol.T ->
  m LLVM.Operand
compileVar sym = do
  Block.externf (Block.internName (NameSymbol.toSymbol sym))

-- TODO :: compile as closures, with captures

-- | Write an LLVM function definition based on a the given lambda abstraction.
-- The function returns the name of the create function.
compileLam ::
  Types.Define m =>
  -- | The type of the lambda abstraction.
  ErasedAnn.Type PrimTy ->
  -- | List of parameter names.
  [NameSymbol.T] ->
  -- | The body of the lambda abstraction.
  Types.Annotated Types.TermClosure ->
  m LLVM.Operand
compileLam ty arguments body = do
    let (llvmArgty, llvmRetty) =
          functionTypeLLVM ty
        llvmArgNames =
          fmap (Block.internName . NameSymbol.toSymbol) arguments
        llvmArguments =
          zip llvmArgty llvmArgNames
    -- time to generate unique names
    lamName <- Block.generateUniqueSymbol "lambda"
    Block.defineFunction llvmRetty lamName llvmArguments $
      do
        bod <- compileTerm body
        Block.ret bod

-- | The function assumes the arguments passed are the arguments of an
-- application.
compileApp ::
  Types.Define m =>
  -- | Application return type
  ErasedAnn.Type PrimTy ->
  -- | The function term of an application.
  Types.Annotated Types.TermClosure ->
  -- | The arguments to the application.
  [Types.Annotated Types.TermClosure] ->
  m LLVM.Operand
compileApp returnTy f@Types.Ann {term} xs =
  case term of
    -- we only treat prims specially
    Types.Prim prim ->
      compilePrimApp returnTy prim xs
    _ -> do
      arguments <- traverse compileTerm xs
      function <- compileTerm f
      --
      let -- We should probably get the type from the function itself
          -- rather than pass in what it should be here
          functionType = typeToLLVM returnTy
          -- ignore attributes for now!
          argsAtrributes = zip arguments (repeat [])
      --
      Block.call functionType function argsAtrributes

compilePrimApp ::
  Types.Define m =>
  -- | Return type of the application
  ErasedAnn.Type PrimTy ->
  -- | The function primitive of the application.
  RawPrimVal ->
  -- | The arguments to the application.
  [Types.Annotated Types.TermClosure] ->
  m LLVM.Operand
compilePrimApp ty f xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm (xs P.!! 0)
        y <- compileTerm (xs P.!! 1)
        Block.add (typeToLLVM ty) x y
      Sub -> do
        x <- compileTerm (xs P.!! 0)
        y <- compileTerm (xs P.!! 1)
        Block.sub (typeToLLVM ty) x y
      Mul -> do
        x <- compileTerm (xs P.!! 0)
        y <- compileTerm (xs P.!! 1)
        Block.mul (typeToLLVM ty) x y
  | otherwise =
    throw @"err"
      ( Types.WrongNumberOfArguments
          ("Was expecting " <> show (arityRaw f) <> "but got " <> show (lengthN xs))
      )

-- | Write LLVM code for a primitive.
-- TODO: implement other primitives.
mkPrim ::
  (Types.Define m) =>
  Monad m =>
  -- | Term that contains the primitive.
  Types.TermClosure ->
  -- | Type of the primitive.
  ErasedAnn.Type PrimTy ->
  m LLVM.Operand
mkPrim (Types.Prim prim) ty = case prim of
  LitInt i -> case ty of
    ErasedAnn.PrimTy (PrimTy LLVM.IntegerType {LLVM.typeBits}) ->
      return $
        LLVM.ConstantOperand $
          LLVM.Int {LLVM.integerBits = typeBits, LLVM.integerValue = i}
  LitString s -> case ty of
    ErasedAnn.PrimTy (PrimTy (LLVM.PointerType (LLVM.IntegerType 8) _)) -> do
      name <- Block.generateUniqueName "LitString"
      Block.globalString (toS s) name

functionTypeLLVM :: ErasedAnn.Type PrimTy -> ([LLVM.Type], LLVM.Type)
functionTypeLLVM prim =
  functionType prim
    |> bimap (fmap typeToLLVM) typeToLLVM

-- | Translate a Juvix type into an LLVM type.
typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty
typeToLLVM ty@(ErasedAnn.Pi _usage _f _xs) =
  Types.pointerOf
    LLVM.FunctionType
      { LLVM.resultType = typeToLLVM resultType,
        LLVM.argumentTypes = map typeToLLVM argumentTypes,
        LLVM.isVarArg = False
      }
  where
    (argumentTypes, resultType) = functionType ty

-- | Construct a tuple of the types of the argument and return type of a function
-- type.
functionType ::
  ErasedAnn.Type primTy ->
  ([ErasedAnn.Type primTy], ErasedAnn.Type primTy)
functionType ty = (init tys, P.last tys)
  where
    tys = functionType' ty
    functionType' (ErasedAnn.Pi _usage l r) = l : functionType' r
    functionType' ty = [ty]
