{-# LANGUAGE OverloadedStrings #-}

module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import Juvix.Library.Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as LLVM (Module, Name, Operand)
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
  ErasedAnn.Prim t' -> mkPrim env t
  ErasedAnn.AppM f xs -> mkApp env (ErasedAnn.term f) ty xs

mkPrim ::
  Env ->
  ErasedAnn.Term PrimTy RawPrimVal ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkPrim _ (ErasedAnn.Prim prim) = case prim of
  LitInt i -> return $ LLVM.int8 i -- TODO deal with the type correctly.

mkApp ::
  Env ->
  -- | The function term of an application.
  ErasedAnn.Term PrimTy RawPrimVal ->
  ErasedAnn.Type PrimTy ->
  -- | The arguments to the application.
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkApp env f (ErasedAnn.PrimTy ty) xs = case f of
  ErasedAnn.Prim prim -> applyPrim env prim ty xs

applyPrim ::
  Env ->
  RawPrimVal ->
  PrimTy ->
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
applyPrim env f ty xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVM.add x y

typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty
