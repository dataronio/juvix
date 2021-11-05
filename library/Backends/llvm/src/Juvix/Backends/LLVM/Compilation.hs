module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import qualified Data.String as S (fromString)
import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as LLVM (Name, Operand (..))
import qualified LLVM.AST.Constant as LLVM (Constant (..))
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder as LLVMBuild
import qualified LLVM.Pretty as LLVM
import qualified Prelude as P

-- | A mapping between Juvix variable names and their operands in LLVM.
type Env = Map.Map NameSymbol.T LLVM.Operand

-- | Compile the input program to an LLVM module.
-- TODO: maybe do something smarter with the module name?
compileProgram ::
  Monad m =>
  -- | Term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  Feedback.FeedbackT [] P.String m Text
compileProgram t = do
  let llvmmod = LLVMBuild.buildModule "juvix-module" $ mkMain t
  return $ toStrict $ LLVM.ppllvm llvmmod

--------------------------------------------------------------------------------
-- Top Level Compilation
--------------------------------------------------------------------------------

compileProgram' ::
  Monad m =>
  -- | Term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  Feedback.FeedbackT [] P.String m Text
compileProgram' t =
  Block.execEnvState (mkMain' t) mempty
    |> Types.moduleAST
    |> LLVM.ppllvm
    |> toStrict
    |> return

--------------------------------------------------------------------------------
-- Function Declaration
--------------------------------------------------------------------------------

mkMain' t@(ErasedAnn.Ann usage ty t') = do
  let (paramTys, returnTy) = functionType ty
      returnTy' = typeToLLVM returnTy
      paramTys' = map typeToLLVM paramTys
      paramNames =
        zipWith
          (\l r -> Block.internName (l <> r))
          (replicate (length paramTys') "arg")
          (fmap (intern . show) [1 ..])
      params = zip paramTys' paramNames
  Block.defineFunction returnTy' "main" params $
    do
      undefined

--------------------------------------------------------------------------------
-- Term Handling
--------------------------------------------------------------------------------

compileTerm' (ErasedAnn.Ann _usage ty t) =
  case t of
    ErasedAnn.LamM caps as body -> compileLam ty caps as body
    ErasedAnn.AppM fn arguments -> compileApp ty fn arguments
    ErasedAnn.Var sym -> compileVar sym
    ErasedAnn.Prim _t -> mkPrim t ty

compileVar ::
  (HasState "symTab" Types.SymbolTable m, HasThrow "err" Types.Errors m) =>
  NameSymbol.T ->
  m LLVM.Operand
compileVar sym = do
  Block.externf (Block.internName (NameSymbol.toSymbol sym))

-- TODO :: compile as closures, with captures
compileLam ty captures arguments body
  | length captures == 0 = do
    let (llvmArgty, llvmRetty) =
          functionTypeLLVM ty
        llvmArgNames =
          fmap (Block.internName . NameSymbol.toSymbol) arguments
        llvmArguments =
          zip llvmArgty llvmArgNames
    -- time to generate unique names
    lamName <- Block.generateUniqueSymbol "lam"
    Block.defineFunction llvmRetty lamName llvmArguments $
      do
        undefined
  | otherwise =
    throw @"err" (Types.UnsupportedOperation "closures are not supported")

compileApp returnTy f@ErasedAnn.Ann {ErasedAnn.term, ErasedAnn.type'} xs =
  case term of
    -- we only treat prims specially
    ErasedAnn.Prim prim ->
      undefined
    _ -> do
      arguments <- traverse compileTerm' xs
      function <- compileTerm' f
      --
      let -- We should probably get the type from the function itself
          -- rather than pass in what it should be here
          functionType = typeToLLVM returnTy
          -- ignore attributes for now!
          argsAtrributes = zip arguments (repeat [])
      --
      Block.call functionType function argsAtrributes

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
  LLVMBuild.MonadModuleBuilder m =>
  -- | Term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m LLVM.Operand
mkMain t@(ErasedAnn.Ann usage ty t') = do
  let (paramTys, returnTy) = functionType ty
      returnTy' = typeToLLVM returnTy
      paramTys' = map typeToLLVM paramTys
      paramNames = repeat "arg"
      params = zip paramTys' (map mkParameterName paramNames)
  LLVMBuild.function "main" params returnTy' $ \args -> do
    out <- case t' of
      ErasedAnn.LamM
        { ErasedAnn.capture,
          ErasedAnn.arguments,
          ErasedAnn.body
        } -> do
          let env = Map.fromList $ zip paramNames args
              callArgs = zip args (repeat []) -- No arg attributes.
          funname <- mkLam env ty body arguments capture
          LLVMBuild.call (globalRef (typeToLLVM ty) funname) callArgs
      _ -> compileTerm mempty t
    LLVMBuild.ret out

-- | Compile a term to its equivalent LLVM code.
-- TODO: implement other constructors of ErasedAnn.Term
compileTerm ::
  (LLVMBuild.MonadIRBuilder m, LLVMBuild.MonadModuleBuilder m) =>
  -- | Environment of Juvix variables to LLVM function arguments.
  Env ->
  -- | The term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m LLVM.Operand
compileTerm env (ErasedAnn.Ann usage ty t) = case t of
  ErasedAnn.Var symbol -> case Map.lookup symbol env of
    Nothing ->
      P.error $ "Variable not found: " <> show symbol <> " in " <> show env -- TODO improve error message.
    Just var -> return var
  ErasedAnn.Prim t' -> mkPrim t ty
  ErasedAnn.AppM f xs -> mkApp env f ty xs

-- | Write an LLVM function definition based on a the given lambda abstraction.
-- The function returns the name of the create function.
mkLam ::
  (LLVMBuild.MonadIRBuilder m, LLVMBuild.MonadModuleBuilder m) =>
  -- | Environment of Juvix variables to LLVM function arguments.
  Env ->
  -- | The type of the lambda abstraction.
  ErasedAnn.Type PrimTy ->
  -- | The body of the lambda abstraction.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  -- | List of parameter names.
  [NameSymbol.T] ->
  -- | List of captures variables (free variables for the body).
  [NameSymbol.T] ->
  m LLVM.Name
mkLam env ty body args capt = do
  funname <- LLVMBuild.freshName "lam"
  let (argTys, returnTy) = functionType ty
      returnTy' = typeToLLVM returnTy
      paramNames = repeat "arg"
      params = zipWith mkParameter argTys paramNames
  LLVMBuild.function funname params returnTy' $ \refs -> do
    let env' = env `Map.union` Map.fromList (zip args refs)
    body' <- compileTerm env' body
    LLVMBuild.ret body'
  return funname

-- | Given a juvix name and type, construct an llvm function parameter
-- definition.
mkParameter ::
  -- | The type of the parameter.
  ErasedAnn.Type PrimTy ->
  -- | The name of the parameter.
  NameSymbol.T ->
  (LLVM.Type, LLVMBuild.ParameterName)
mkParameter ty name = (typeToLLVM ty, mkParameterName name)

-- | The function assumes the arguments passed are the arguments of an
-- application.
mkApp ::
  (LLVMBuild.MonadIRBuilder m, LLVMBuild.MonadModuleBuilder m) =>
  -- | Environment of Juvix variables to LLVM function arguments.
  Env ->
  -- | The function term of an application.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  -- | The type of the application.
  ErasedAnn.Type PrimTy ->
  -- | The arguments to the application.
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  m LLVM.Operand
mkApp env f@(ErasedAnn.Ann {ErasedAnn.term, ErasedAnn.type'}) _ xs =
  case term of
    ErasedAnn.LamM {ErasedAnn.body, ErasedAnn.arguments, ErasedAnn.capture} -> do
      funname <- mkLam env type' body arguments capture
      xs' <- mapM (compileTerm env) xs
      let xs'args = zip xs' (repeat [])
      LLVMBuild.call (globalRef (typeToLLVM type') funname) xs'args
    ErasedAnn.Prim prim -> applyPrim env prim xs
    ErasedAnn.Var v -> do
      f' <- compileTerm env f
      xs' <- mapM (compileTerm env) xs
      let xs'args = zip xs' (repeat []) -- Do not pass attributes to the args.
      LLVMBuild.call f' xs'args

-- | Write LLVM code for a primitive.
-- TODO: implement other primitives.
mkPrim ::
  Monad m =>
  -- | Term that contains the primitive.
  ErasedAnn.Term PrimTy RawPrimVal ->
  -- | Type of the primitive.
  ErasedAnn.Type PrimTy ->
  m LLVM.Operand
mkPrim (ErasedAnn.Prim prim) ty = case prim of
  LitInt i -> case ty of
    ErasedAnn.PrimTy (PrimTy LLVM.IntegerType {LLVM.typeBits}) ->
      return $
        LLVM.ConstantOperand $
          LLVM.Int {LLVM.integerBits = typeBits, LLVM.integerValue = i}

-- | Generate code for primitives that are used as a function.
-- TODO: implement other primitives.
applyPrim ::
  (LLVMBuild.MonadIRBuilder m, LLVMBuild.MonadModuleBuilder m) =>
  -- | Environment of global variables.
  Env ->
  -- | The function primitive of the application.
  RawPrimVal ->
  -- | The arguments to the application.
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  m LLVM.Operand
applyPrim env f xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVMBuild.add x y
      Sub -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVMBuild.sub x y
      Mul -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVMBuild.mul x y

-- | Counterpart of `mkName`: make a `ParameterName` given a name.
mkParameterName :: NameSymbol.T -> LLVMBuild.ParameterName
mkParameterName s = S.fromString $ unintern $ NameSymbol.toSymbol s

-- | Handy wrapper for creating global references based on a type and name.
globalRef :: LLVM.Type -> LLVM.Name -> LLVM.Operand
globalRef ty name = LLVM.ConstantOperand $ LLVM.GlobalReference ty name

functionTypeLLVM prim =
  functionType prim
    |> bimap (fmap typeToLLVM) typeToLLVM

-- | Translate a Juvix type into an LLVM type.
typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty
typeToLLVM ty@(ErasedAnn.Pi _usage _f _xs) =
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
    functionType' (ErasedAnn.Pi usage l r) = l : functionType' r
    functionType' ty = [ty]
