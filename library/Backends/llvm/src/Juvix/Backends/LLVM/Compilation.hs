{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import qualified Data.String as S (fromString)
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import Juvix.Library.Feedback
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as LLVM (Module, Name, Operand (..))
import qualified LLVM.AST.Constant as LLVM (Constant (..))
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder as LLVM
import qualified LLVM.Pretty as LLVM
import qualified Prelude as P

compileProgram ::
  Monad m =>
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  FeedbackT [] P.String m Text
compileProgram t = do
  let llvmmod :: LLVM.Module
      llvmmod = LLVM.buildModule "juvix-module" $ mkMain t
  return $ toStrict $ LLVM.ppllvm llvmmod

mkMain ::
  LLVM.MonadModuleBuilder m =>
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m LLVM.Operand
mkMain t@(ErasedAnn.Ann usage ty t') = do
  let types = map typeToLLVM (functionTy ty)
      returnType = P.last types
      paramTypes = init types
      paramNames = repeat "arg"
      params = zip paramTypes (map mkParameterName paramNames)
  LLVM.function "main" params returnType $ \args -> do
    let env = Map.fromList $ zip paramNames args -- Bind names with arguments.
    out <- case t' of
      ErasedAnn.LamM
        { ErasedAnn.capture,
          ErasedAnn.arguments,
          ErasedAnn.body
        } -> do
          funname <- mkLam env ty body arguments capture
          let callArgs = zip args (repeat []) -- No arg attributes.
          LLVM.call (globalRef (typeToLLVM ty) funname) callArgs
      _ -> do
        compileTerm env t
    LLVM.ret out

type Env = Map.Map NameSymbol.T LLVM.Operand

-- | Counterpart of `mkName`: make a `ParameterName` given a name.
mkParameterName :: NameSymbol.T -> LLVM.ParameterName
mkParameterName s = S.fromString $ unintern $ NameSymbol.toSymbol s -- S.fromString _

compileTerm ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  Env ->
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m LLVM.Operand
compileTerm env (ErasedAnn.Ann usage ty t) = case t of
  ErasedAnn.Var symbol -> case Map.lookup symbol env of
    Nothing ->
      P.error $ "Variable not found: " <> show symbol <> " in " <> show env -- TODO improve error message.
    Just var -> return var
  ErasedAnn.Prim t' -> mkPrim t ty
  ErasedAnn.AppM f xs -> mkApp env f ty xs

-- | Write LLVM code for a primitive.
mkPrim ::
  LLVM.MonadIRBuilder m =>
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

-- | Write an LLVM function definition based on a the given lambda abstraction.
-- The function returns the name of the create function.
mkLam ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
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
  funname <- LLVM.freshName "lam"
  let returnTy = typeToLLVM $ ErasedAnn.type' body
  params <- mapM mkParam (zip args (functionTy ty))
  LLVM.function funname params returnTy $ \refs -> do
    let env' = env `Map.union` Map.fromList (zip args refs)
    body' <- compileTerm env' body
    LLVM.ret body'
  return funname
  where
    -- Given a Juvix name and type, construct an LLVM function parameter
    -- definition.
    mkParam ::
      LLVM.MonadIRBuilder m =>
      (NameSymbol.T, ErasedAnn.Type PrimTy) ->
      m (LLVM.Type, LLVM.ParameterName)
    mkParam (name, ty) = do
      let ty' = typeToLLVM ty
          name' = unintern $ NameSymbol.toSymbol name
      return $ (ty', S.fromString $ "arg" <> name')

-- Construct a list of types from a function type.
functionTy :: ErasedAnn.Type primTy -> [ErasedAnn.Type primTy]
functionTy (ErasedAnn.Pi usage l r) = l : functionTy r
functionTy ty = [ty]

-- | The function assumes the arguments passed are the arguments of an
-- application.
mkApp ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  -- | Environment of global variables.
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
      LLVM.call (globalRef (typeToLLVM type') funname) xs'args
    ErasedAnn.Prim prim -> applyPrim env prim xs
    ErasedAnn.Var v -> do
      f' <- compileTerm env f
      xs' <- mapM (compileTerm env) xs
      let xs'args = zip xs' (repeat []) -- Do not pass attributes to the args.
      LLVM.call f' xs'args

applyPrim ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  Env ->
  RawPrimVal ->
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  m LLVM.Operand
applyPrim env f xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVM.add x y

-- | Translate a Juvix type into an LLVM type.
typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty
typeToLLVM (ErasedAnn.Pi _usage f xs) =
  LLVM.FunctionType
    { LLVM.resultType = typeToLLVM resultType,
      LLVM.argumentTypes = map typeToLLVM argumentTypes,
      LLVM.isVarArg = False
    }
  where
    tyList = f : functionTy xs
    (resultType : revArgumentTypes) = reverse tyList
    argumentTypes = reverse revArgumentTypes

-- | Handy wrapper for creating global references based on a type and name.
globalRef :: LLVM.Type -> LLVM.Name -> LLVM.Operand
globalRef ty name = LLVM.ConstantOperand $ LLVM.GlobalReference ty name
