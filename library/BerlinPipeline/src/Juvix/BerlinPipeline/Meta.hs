module Juvix.BerlinPipeline.Meta
  ( Feedback,
    Trace,
    T,
  )
where

import Control.Monad (ap)
import Juvix.Library

data Feedback
  deriving (Eq, Show)

data Trace
  deriving (Eq, Show)

data T = Meta
  { feedback :: Feedback,
    trace :: Trace
  }
  deriving (Eq, Show)
