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
compileProgram t@(ErasedAnn.Ann usage ty _) = do
  let llvmmod :: LLVM.Module
      llvmmod = LLVM.buildModule "juvix-module" $ do
        LLVM.function "main" [] (typeToLLVM ty) $ \[] -> do
          out <- compileTerm [] t
          LLVM.ret out
  return $ toStrict $ LLVM.ppllvm llvmmod

type Env = [(NameSymbol.T, LLVM.Operand)]

compileTerm ::
  Env ->
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
compileTerm env (ErasedAnn.Ann usage ty t) = case t of
  ErasedAnn.Var symbol -> case P.lookup symbol env of
    Nothing -> P.error "Variable not found." -- TODO improve error message.
    Just var -> return var
  ErasedAnn.Prim t' -> mkPrim t ty
  ErasedAnn.AppM f xs -> mkApp env f ty xs

-- | Write LLVM code for a primitive.
mkPrim ::
  -- | Term that contains the primitive.
  ErasedAnn.Term PrimTy RawPrimVal ->
  -- | Type of the primitive.
  ErasedAnn.Type PrimTy ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkPrim (ErasedAnn.Prim prim) ty = case prim of
  LitInt i -> case ty of
    ErasedAnn.PrimTy (PrimTy LLVM.IntegerType {LLVM.typeBits}) ->
      return $
        LLVM.ConstantOperand $
          LLVM.Int {LLVM.integerBits = typeBits, LLVM.integerValue = i}

-- | Write an LLVM function definition based on a the given lambda abstraction.
-- The function returns the name of the create function.
mkLam ::
  Env ->
  -- | The type of the lambda abstraction.
  ErasedAnn.Type PrimTy ->
  -- | The body of the lambda abstraction.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  -- | List of parameter names.
  [NameSymbol.T] ->
  -- | List of captures variables (free variables for the body).
  [NameSymbol.T] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Name
mkLam env ty body args capt = do
  funname <- LLVM.freshName "lam"
  let returnTy = typeToLLVM $ ErasedAnn.type' body
  params <- mapM mkParam (zip args (functionTy ty))
  LLVM.function funname params returnTy $ \refs -> do
    let env' = (zip args refs) ++ env
    body' <- lift $ compileTerm env' body
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
      name' <- LLVM.fresh
      let ty' = typeToLLVM ty
      return $ (ty', S.fromString $ show name')

    -- Construct a list of types from a function type.
    functionTy :: ErasedAnn.Type primTy -> [ErasedAnn.Type primTy]
    functionTy (ErasedAnn.Pi usage l r) = l : functionTy r
    functionTy ty = [ty]

-- | The function assumes the arguments passed are the arguments of an
-- application.
mkApp ::
  -- | Environment of global variables.
  Env ->
  -- | The function term of an application.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  -- | The type of the application.
  ErasedAnn.Type PrimTy ->
  -- | The arguments to the application.
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkApp env f@(ErasedAnn.Ann {ErasedAnn.term, ErasedAnn.type'}) _ xs =
  case term of
    ErasedAnn.LamM {ErasedAnn.body, ErasedAnn.arguments, ErasedAnn.capture} -> do
      funname <- mkLam env type' body arguments capture
      LLVM.call (LLVM.ConstantOperand $ LLVM.GlobalReference (typeToLLVM type') funname) []
    ErasedAnn.Prim prim -> applyPrim env prim xs
    ErasedAnn.Var v -> do
      f' <- compileTerm env f
      xs' <- mapM (compileTerm env) xs
      let xs'args = zip xs' (repeat []) -- Do not pass attributes to the args.
      LLVM.call f' xs'args

applyPrim ::
  Env ->
  RawPrimVal ->
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
applyPrim env f xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVM.add x y

typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty
