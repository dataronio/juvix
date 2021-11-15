module Juvix.BerlinPipeline.Step
  ( Named,
    StepMetaInfo,
    register,
  )
where

import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T (Pipeline.ComputationalInput -> Pipeline.ComputationalOutput Pipeline.WorkingEnv)

data Named = Named
  { nsName :: NameSymbol.T,
    nsStep :: T
  }

data StepMetaInfo = StepMetaInfo
  { name :: Maybe NameSymbol.T,
    meta :: Meta.T
  }

register :: NameSymbol.T -> T -> Named
register name func = Named name func
