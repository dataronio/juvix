{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Juvix.Playground.HTTP.Routes where

import qualified Control.Monad.State.Lazy as State
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Context as Context
import qualified Juvix.Core.Base as Base
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as Typechecker
import qualified Juvix.Core.Parameterisation as Parameterisation
import qualified Juvix.Frontend.Types as Frontend
import Juvix.Library hiding (All)
import Juvix.Library.BLS12381 (Fr)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Sexp as Sexp
import Servant
import Text.Pretty.Simple (pShowNoColor)

type Result a = Feedback.Feedback [] [Char] a

data Backend
  = Plonk (Plonk.BPlonk Fr)
  | LLVM LLVM.BLLVM
  | Michelson Michelson.BMichelson
  deriving (Eq, Show, Generic, A.ToJSON)

instance A.FromJSON Backend where
  parseJSON (A.String "LLVM") = pure (LLVM LLVM.BLLVM)
  parseJSON (A.String "Plonk") = pure (Plonk Plonk.BPlonk)
  parseJSON (A.String "Michelson") = pure (Michelson Michelson.BMichelson)

data Req = Req {code :: Text, backend :: Backend}
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

type Parse =
  Description "Parse Juvix source code"
    :> ReqBody '[JSON] Req
    :> Post '[JSON] ([[Char]], AllSteps)

type Typecheck =
  Description "Typecheck Juvix source code"
    :> ReqBody '[JSON] Req
    :> Post '[JSON] ([[Char]], AllSteps)

type Compile =
  Description "Compile Juvix source code"
    :> ReqBody '[JSON] Req
    :> Post '[JSON] ([[Char]], AllSteps)

type Pipeline =
  "pipeline"
    :> ( ("parse" :> Parse)
           :<|> ("typecheck" :> Typecheck)
           :<|> ("compile" :> Compile)
       )

pipeline :: Server Pipeline
pipeline =
  parse
    :<|> typecheck
    :<|> compile

data AllSteps = AllSteps
  { allToML :: Maybe Text,
    allToSexp :: Maybe Text,
    allToHR :: Maybe Text,
    allToIR :: Maybe Text,
    allToErased :: Maybe Text,
    allToBackend :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance A.ToJSON (AllSteps) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON (AllSteps) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

emptyAllSteps :: AllSteps
emptyAllSteps = AllSteps Nothing Nothing Nothing Nothing Nothing Nothing

continueSuccess ::
  MonadIO m =>
  (t -> State.StateT (app msg, AllSteps) m ()) ->
  (t -> Feedback.FeedbackT app msg IO t') ->
  (t -> t) ->
  Feedback.Feedback app msg t ->
  State.StateT (app msg, AllSteps) m (Feedback.Feedback app msg t')
continueSuccess update next filterPrelude f = case f of
  Feedback.Success _ m -> do
    update $ filterPrelude m
    liftIO . Feedback.runFeedbackT $ next m
  Feedback.Fail err -> do
    State.modify (\(_, s) -> (err, s))
    pure $ Feedback.Fail err

parseSt ::
  (MonadIO m, Pipeline.HasBackend b) =>
  Text ->
  b ->
  StateT ([[Char]], AllSteps) m (Result (Context.T Sexp.T Sexp.T Sexp.T))
parseSt script backend = do
  mlF <-
    liftIO . Feedback.runFeedbackT $
      Pipeline.toML backend script
  sexpF <- continueSuccess modifyML (Pipeline.toSexp backend) filterML mlF
  case sexpF of
    Feedback.Success _ m -> do
      modifySexp $ filterSexp m
    Feedback.Fail err -> do
      State.modify (\(_, s) -> (err, s))
  pure sexpF
  where
    modifyML r = State.modify (\(e, s) -> (e, s {allToML = Just $ toS $ pShowNoColor r}))
    modifySexp r = State.modify (\(e, s) -> (e, s {allToSexp = Just $ toS $ pShowNoColor r}))

    filterML = filter (\(name, _) -> "Prelude" `elem` name)
    filterSexp = identity

typecheckSt script b = do
  erasedF <-
    parseSt script b
      >>= continueSuccess modifySexp (Pipeline.toHR (Pipeline.param b)) filterSexp
      >>= continueSuccess modifyHR Pipeline.toIR filterHR
      >>= continueSuccess modifyIR (Pipeline.toErased (Pipeline.param b)) filterIR
  case erasedF of
    Feedback.Success _ m -> do
      modifyErased $ filterErased m
    Feedback.Fail err -> do
      State.modify (\(_, s) -> (err, s))
  pure erasedF
  where
    modifySexp r = State.modify (\(e, s) -> (e, s {allToSexp = Just $ toS $ pShowNoColor r}))
    modifyHR r = State.modify (\(e, s) -> (e, s {allToHR = Just $ toS $ pShowNoColor r}))
    modifyIR r = State.modify (\(e, s) -> (e, s {allToIR = Just $ toS $ pShowNoColor r}))
    modifyErased r = State.modify (\(e, s) -> (e, s {allToErased = Just $ toS $ pShowNoColor r}))

    filterSexp = identity
    filterHR = HM.filterWithKey isNotPrelude
    isNotPrelude (p NonEmpty.:| _) _ = p /= "Prelude"
    filterIR = second (HM.filterWithKey isNotPrelude)
    filterErased = identity

compileSt script backend = do
  circF <-
    typecheckSt script backend
      >>= continueSuccess modifyErased toBackend filterErased
  case circF of
    Feedback.Success _ m -> do
      modifyBackend m
    Feedback.Fail err -> do
      State.modify (\(_, s) -> (err, s))
  pure circF
  where
    toBackend erasedAnn = Pipeline.compile' erasedAnn
    modifyErased r = State.modify (\(e, s) -> (e, s {allToErased = Just $ toS $ pShowNoColor r}))
    modifyBackend r = State.modify (\(e, s) -> (e, s {allToBackend = Just $ toS $ pShowNoColor r}))
    filterErased = identity

parse :: Server Parse
parse (Req script (LLVM b)) = flip execStateT ([], emptyAllSteps) $ parseSt script b
parse (Req script (Plonk b)) = flip execStateT ([], emptyAllSteps) $ parseSt script b
parse (Req script (Michelson b)) = flip execStateT ([], emptyAllSteps) $ parseSt script b

typecheck :: Server Parse
typecheck (Req script (LLVM b)) = flip execStateT ([], emptyAllSteps) $ typecheckSt script b
typecheck (Req script (Plonk b)) = flip execStateT ([], emptyAllSteps) $ typecheckSt script b
typecheck (Req script (Michelson b)) = flip execStateT ([], emptyAllSteps) $ typecheckSt script b

compile :: Server Compile
compile (Req script (LLVM b)) = flip execStateT ([], emptyAllSteps) $ compileSt script b
compile (Req script (Plonk b)) = flip execStateT ([], emptyAllSteps) $ compileSt script b
compile (Req script (Michelson b)) = flip execStateT ([], emptyAllSteps) $ compileSt script b

proxy :: Proxy Pipeline
proxy = Proxy
