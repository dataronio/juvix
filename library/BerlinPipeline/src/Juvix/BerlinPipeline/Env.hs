module Juvix.BerlinPipeline.Env where

import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.RecursiveList as RecursiveList
import qualified Juvix.BerlinPipeline.Step as Step
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T
  { information :: Pipeline.ComputationalInput,
    registeredPipeline :: CircularList.T (Step.Named),
    stoppingStep :: Maybe NameSymbol.T
  }
  deriving (Show, Eq, Generic)

newtype EnvS b = EnvS (State T b)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "information" (Pipeline.ComputationalInput),
      HasSink "information" (Pipeline.ComputationalInput),
      HasSource "information" (Pipeline.ComputationalInput)
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

registerStep :: CircularList.T Step.Named -> EnvS ()
registerStep = notImplemented

defPipelineGroup :: NameSymbol.T -> [CircularList.T Step.Named] -> CircularList.T Step.Named
defPipelineGroup = notImplemented

stopAt :: NameSymbol.T -> EnvS ()
stopAt = notImplemented

stopAtNothing :: EnvS ()
stopAtNothing = notImplemented

eval :: T -> Pipeline.ComputationalInput
eval = notImplemented

run :: EnvS b -> T -> Pipeline.ComputationalInput
run = notImplemented

extract :: EnvS b -> T
extract = notImplemented
