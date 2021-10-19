{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Juvix.Playground.HTTP.Routes where

import qualified Control.Monad.State.Lazy as State
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Context as Context
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Frontend.Types as Frontend
import Juvix.Library hiding (All)
import Juvix.Library.BLS12381 (Fr)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Sexp as Sexp
import Servant

type Result a = Feedback.Feedback [] [Char] a

-- TODO: Include backend details
data Req = Req {reqBody :: Text}
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

type ToML =
  Description "Convert Juvix source code to AST"
    :> ReqBody '[JSON] Req
    :> Post '[JSON] (Result [(NameSymbol.T, [Frontend.TopLevel])])

type ToSexp =
  Description "Convert AST to S-expression"
    :> ReqBody '[JSON] [(NameSymbol.T, [Frontend.TopLevel])]
    :> Post '[JSON] (Result (Context.T Sexp.T Sexp.T Sexp.T))

type ToHR =
  Description "Convert S-expression to HR"
    :> ReqBody '[JSON] (Context.T Sexp.T Sexp.T Sexp.T)
    :> Post '[JSON] (Result (Core.RawGlobals HR.T (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)))

type ToIR =
  Description "Convert HR to IR"
    :> ReqBody '[JSON] (Core.RawGlobals HR.T (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
    :> Post '[JSON] (Result (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)))

type ToErased =
  Description "Convert IR to Erased"
    :> ReqBody '[JSON] (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
    :> Post '[JSON] (Result (ErasedAnn.AnnTermT (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)))

-- TODO: Abstract to any backend
type ToBackend =
  Description "Convert Erased to Backend"
    :> ReqBody '[JSON] (ErasedAnn.AnnTermT (Plonk.PrimTy Fr) (Plonk.PrimVal Fr))
    :> Post '[JSON] (Result (Circuit Fr))

data Circuit f = Circuit
  { circ :: Plonk.ArithCircuit Fr,
    circDot :: Text,
    circPretty :: Text
  }
  deriving (Show, Eq, Generic)

instance A.ToJSON f => A.ToJSON (Circuit f) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON f => A.FromJSON (Circuit f) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

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
    :> ( ("step" :> Steps)
           :<|> ("parse" :> Parse)
           :<|> ("typecheck" :> Typecheck)
           :<|> ("compile" :> Compile)
       )

type Steps =
  "step"
    :> ( ("to-ml" :> ToML)
           :<|> ("to-sexp" :> ToSexp)
           :<|> ("to-hr" :> ToHR)
           :<|> ("to-ir" :> ToIR)
           :<|> ("to-erased" :> ToErased)
           :<|> ("to-circuit" :> ToBackend)
       )

juvixRootPath :: FilePath
juvixRootPath = "../../"

libs :: IsString a => [a]
libs = ["stdlib/Prelude.ju", "stdlib/Circuit.ju"]

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

pipeline :: Server Pipeline
pipeline =
  steps
    :<|> parse
    :<|> typecheck
    :<|> compile

-- TODO: Make it backend agnostic
data AllSteps = AllSteps
  { allToML :: Maybe [(NameSymbol.T, [Frontend.TopLevel])],
    allToSexp :: Maybe (Context.T Sexp.T Sexp.T Sexp.T),
    allToHR :: Maybe (Core.RawGlobals HR.T (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)),
    allToIR :: Maybe (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)),
    allToErased :: Maybe (ErasedAnn.AnnTermT (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)),
    allToBackend :: Maybe (Circuit Fr)
  }
  deriving (Show, Eq, Generic)

instance A.ToJSON AllSteps where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON AllSteps where
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
  MonadIO m =>
  Req ->
  StateT ([[Char]], AllSteps) m (Result (Context.T Sexp.T Sexp.T Sexp.T))
parseSt (Req script) = do
  mlF <-
    liftIO . Feedback.runFeedbackT $
      Pipeline.toML' (withJuvixRootPath <$> libs) (Plonk.BPlonk @Fr) script
  sexpF <- continueSuccess modifyML (Pipeline.toSexp (Plonk.BPlonk @Fr)) filterML mlF
  case sexpF of
    Feedback.Success _ m -> do
      modifySexp $ filterSexp m
    Feedback.Fail err -> do
      State.modify (\(_, s) -> (err, s))
  pure sexpF
  where
    modifyML r = State.modify (\(e, s) -> (e, s {allToML = Just r}))
    modifySexp r = State.modify (\(e, s) -> (e, s {allToSexp = Just r}))

    filterML = filter (\(name, _) -> "Prelude" `elem` name)
    filterSexp = identity

typecheckSt ::
  MonadIO m =>
  Req ->
  StateT ([[Char]], AllSteps) m (Result (ErasedAnn.AnnTermT (Plonk.PrimTy Fr) (Plonk.PrimVal Fr)))
typecheckSt req = do
  erasedF <-
    parseSt req
      >>= continueSuccess modifySexp (Pipeline.toHR (Plonk.param @Fr)) filterSexp
      >>= continueSuccess modifyHR Pipeline.toIR filterHR
      >>= continueSuccess modifyIR (Pipeline.toErased (Plonk.param @Fr)) filterIR
  case erasedF of
    Feedback.Success _ m -> do
      modifyErased $ filterErased m
    Feedback.Fail err -> do
      State.modify (\(_, s) -> (err, s))
  pure erasedF
  where
    modifySexp r = State.modify (\(e, s) -> (e, s {allToSexp = Just r}))
    modifyHR r = State.modify (\(e, s) -> (e, s {allToHR = Just r}))
    modifyIR r = State.modify (\(e, s) -> (e, s {allToIR = Just r}))
    modifyErased r = State.modify (\(e, s) -> (e, s {allToErased = Just r}))

    filterSexp = identity
    filterHR = HM.filterWithKey isNotPrelude
    isNotPrelude (p NonEmpty.:| _) _ = p /= "Prelude"
    filterIR = second (HM.filterWithKey isNotPrelude)
    filterErased = identity

compileSt ::
  MonadIO m =>
  Req ->
  StateT ([[Char]], AllSteps) m (Result (Circuit Fr))
compileSt req = do
  circF <-
    typecheckSt req
      >>= continueSuccess modifyErased toCircuit filterErased
  case circF of
    Feedback.Success _ m -> do
      modifyBackend m
    Feedback.Fail err -> do
      State.modify (\(_, s) -> (err, s))
  pure circF
  where
    toCircuit erasedAnn =
      let circ = Plonk.compileCircuit erasedAnn
       in pure $ Circuit {circ, circPretty = Plonk.prettifyCircuit circ, circDot = Plonk.arithCircuitToDot circ}
    modifyErased r = State.modify (\(e, s) -> (e, s {allToErased = Just r}))
    modifyBackend r = State.modify (\(e, s) -> (e, s {allToBackend = Just r}))
    filterErased = identity

parse :: Server Parse
parse = flip execStateT ([], emptyAllSteps) . parseSt

typecheck :: Server Parse
typecheck = flip execStateT ([], emptyAllSteps) . typecheckSt

compile :: Server Compile
compile = flip execStateT ([], emptyAllSteps) . compileSt

steps :: Server Steps
steps =
  toML
    :<|> toSexp
    :<|> toHR
    :<|> toIR
    :<|> toErased
    :<|> toCircuit
  where
    toML :: Server ToML
    toML = liftIO . Feedback.runFeedbackT . Pipeline.toML' (withJuvixRootPath <$> libs) (Plonk.BPlonk @Fr) . reqBody
    toSexp = liftIO . Feedback.runFeedbackT . Pipeline.toSexp (Plonk.BPlonk @Fr)
    toHR = liftIO . Feedback.runFeedbackT . Pipeline.toHR (Plonk.param @Fr)
    toIR = liftIO . Feedback.runFeedbackT . Pipeline.toIR
    toErased = liftIO . Feedback.runFeedbackT . Pipeline.toErased (Plonk.param @Fr)
    toCircuit erasedAnn =
      let circ = Plonk.compileCircuit erasedAnn
       in pure . Feedback.Success [] $ Circuit {circ, circPretty = Plonk.prettifyCircuit circ, circDot = Plonk.arithCircuitToDot circ}

proxy :: Proxy Pipeline
proxy = Proxy
