{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The basic connection between the backend and the Juvix pipeline.
module Juvix.Backends.LLVM.Pipeline
  ( BLLVM (..),
  )
where

import qualified Data.Aeson as A
import Juvix.Backends.LLVM.Compilation
import Juvix.Backends.LLVM.Parameterization
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Pipeline as Pipeline

-- | Identifier for the LLVM backend.
data BLLVM = BLLVM
  deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)

instance Pipeline.HasBackend BLLVM where
  type Ty BLLVM = PrimTy
  type Val BLLVM = RawPrimVal
  type Err BLLVM = CompilationError

  stdlibs _ = ["LLVM.ju", "LLVM/Int.ju"]

  param _ = llvm

  typecheck ctx = Pipeline.typecheck' ctx llvm

  compile' term = do
    let raw = ErasedAnn.toRaw term
    compileProgram raw
