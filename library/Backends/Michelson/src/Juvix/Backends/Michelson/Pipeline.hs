{-# OPTIONS_GHC -fdefer-type-errors #-}

module Juvix.Backends.Michelson.Pipeline (BMichelson (..), compileMichelson) where

import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline as Pipeline

data BMichelson = BMichelson
  deriving (Eq, Show)

instance HasBackend BMichelson where
  type Ty BMichelson = Param.RawPrimTy
  type Val BMichelson = Param.RawPrimVal
  type Err BMichelson = Param.CompilationError

  stdlibs _ = ["stdlib/Michelson.ju", "stdlib/MichelsonAlias.ju"]

  typecheck ctx = Pipeline.typecheck' ctx Param.michelson Param.Set

  compile out term = do
    let (res, _logs) = M.compileContract $ ErasedAnn.toRaw term
    case res of
      Right c -> do
        writeout out $ M.untypedContractToSource (fst c)
      Left err -> Feedback.fail $ show err

compileMichelson ::
  MonadFail f =>
  Param.AnnTerm
    Param.RawPrimTy
    (ErasedAnn.TypedPrim Param.RawPrimTy Param.RawPrimVal) ->
  f Text
compileMichelson term = do
  let (res, _logs) = M.compileContract $ ErasedAnn.toRaw term
  case res of
    Right c -> pure $ M.untypedContractToSource (fst c)
    Left err -> Feedback.fail $ show err
