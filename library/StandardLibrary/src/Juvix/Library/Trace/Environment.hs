{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module gives a minimal environment to run Traces, best used
-- for testing
module Juvix.Library.Trace.Environment where

import Juvix.Library
import Juvix.Library.Trace.Types

newtype Minimal = Minimal
  { trace :: T
  }
  deriving (Generic, Show)

type MinimalAlias = State Minimal

type MinimalAliasIO = StateT Minimal IO

newtype MinimalM a = Ctx {_run :: MinimalAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "trace" T,
      HasSource "trace" T,
      HasSink "trace" T
    )
    via StateField "trace" MinimalAlias

newtype MinimalMIO a = CtxIO (MinimalAliasIO a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "trace" T,
      HasSource "trace" T,
      HasSink "trace" T
    )
    via StateField "trace" MinimalAliasIO

run :: MinimalM a -> Minimal -> (a, Minimal)
run (Ctx c) = runState c

runIO :: MinimalMIO a -> Minimal -> IO (a, Minimal)
runIO (CtxIO c) = runStateT c
