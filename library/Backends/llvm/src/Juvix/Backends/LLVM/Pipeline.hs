{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The basic connection between the backend and the Juvix pipeline.
module Juvix.Backends.LLVM.Pipeline
  ( BLLVM (..),
  )
where

import qualified Data.Aeson as A
import qualified Juvix.Backends.LLVM.Compilation as Compiliation
import qualified Juvix.Backends.LLVM.Parameterization as Parameterization
import qualified Juvix.Backends.LLVM.Primitive as Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Pipeline as Pipeline

-- | Identifier for the LLVM backend.
data BLLVM = BLLVM
  deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)

instance Pipeline.HasBackend BLLVM where
  type Ty BLLVM = Primitive.PrimTy
  type Val BLLVM = Primitive.RawPrimVal
  type Err BLLVM = Primitive.CompilationError

  stdlibs _ = ["LLVM.ju", "LLVM/Int.ju"]

  param _ = Parameterization.llvm

  typecheck ctx = Pipeline.typecheck' ctx Parameterization.llvm

  compile' term = do
    let raw = ErasedAnn.toRaw term
    Compiliation.compileProgram raw
