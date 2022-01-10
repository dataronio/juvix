{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Env where

import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T
  { information :: Pipeline.CIn,
    registeredPipeline :: CircularList.T (Step.Named),
    stoppingStep :: Maybe NameSymbol.T
  }
  deriving (Generic)

newtype EnvS b = EnvS (State T b)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "information" (Pipeline.CIn),
      HasSink "information" (Pipeline.CIn),
      HasSource "information" (Pipeline.CIn)
    )
    via StateField "information" (State T)
  deriving
    ( HasState "registeredPipeline" (CircularList.T (Step.Named)),
      HasSink "registeredPipeline" (CircularList.T (Step.Named)),
      HasSource "registeredPipeline" (CircularList.T (Step.Named))
    )
    via StateField "registeredPipeline" (State T)
  deriving
    ( HasState "stoppingStep" (Maybe NameSymbol.T),
      HasSink "stoppingStep" (Maybe NameSymbol.T),
      HasSource "stoppingStep" (Maybe NameSymbol.T)
    )
    via StateField "stoppingStep" (State T)

data StopADT = Stop

-- | Register the pipeline function to the environment
registerStep :: CircularList.T Step.Named -> EnvS ()
registerStep l = do
  modify @"registeredPipeline" $ \i -> i <> l

-- | Create a named group of pipeline steps or nested grouping of pipeline steps.
defPipelineGroup :: NameSymbol.T -> [CircularList.T Step.Named] -> CircularList.T Step.Named
defPipelineGroup sym ls = foldl' (<>) (CircularList.init sym) ls

-- | Tell the environment to stop at a particular step when running the environment.
stopAt :: NameSymbol.T -> EnvS ()
stopAt sym = put @"stoppingStep" (Just sym)

stopAtNothing :: EnvS ()
stopAtNothing = put @"stoppingStep" Nothing

-- Change our environment
eval :: MonadIO m => T -> m Pipeline.CIn
eval
  ( T
      input@(Pipeline.CIn wEnv@(Pipeline.WorkingEnv sexp context) surr)
      pipeline
      stoppingStep
    ) = do
    case nextStep of
      Nothing -> pure input
      Just (CircularList.NonCircSchema nStep) -> do
        let name = Step.name nStep
        let namedInput = Pipeline.nameCIn name input
        if shouldStop stoppingStep name
          then pure input
          else liftIO $ do
            let (Step.T step) = Step.step nStep
            res <- step namedInput
            updateOnStep remainder stoppingStep namedInput res
      Just (CircularList.CircSchema ls) -> notImplemented
    where
      shouldStop (Just n) named
        | n == named = True
        | otherwise = False
      shouldStop _ _ = False
      nextStep = CircularList.firstNested pipeline
      remainder = CircularList.removeFirstNested pipeline

updateOnStep ::
  MonadIO m =>
  CircularList.T (Step.Named) ->
  Maybe NameSymbol.T ->
  Pipeline.CIn ->
  Pipeline.COut Pipeline.WorkingEnv ->
  m Pipeline.CIn
updateOnStep pipeline stoppingStep namedInput res =
  case res of
    Pipeline.Success {meta, result} ->
      eval $
        T
          { information =
              Pipeline.CIn
                { languageData = result,
                  surroundingData = Pipeline.SurroundingEnv Nothing meta
                },
            registeredPipeline = pipeline,
            stoppingStep
          }
    Pipeline.Failure {meta, partialResult} -> pure $ case partialResult of
      Nothing -> Pipeline.metaCIn meta namedInput
      Just wEnv ->
        let out = Pipeline.metaCIn meta namedInput
         in out {Pipeline.languageData = wEnv}

run :: EnvS b -> T -> Pipeline.CIn
run (EnvS st) = information . execState st
