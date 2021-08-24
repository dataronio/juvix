{-# LANGUAGE TemplateHaskell #-}

-- | The Trace library represents the ability to properly trace code
-- throughout Haskell.
--
-- - The structure requires your code to exist in the =Trace.TEff=
--   effect.
module Juvix.Library.Trace where

import Control.Lens (over, set, (^.))
import qualified Control.Lens as Lens hiding ((|>))
import qualified Data.Text as T
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T
  { -- | @tCurrent@ represents the current stack-trace we are computing
    -- under, We use a StackChain to determine the trace the current
    -- trace is computing under.
    tCurrent :: StackChain,
    -- | @tTraces@ represents our current trace log of all completed
    -- functions
    tTraces :: Log,
    -- | @tEnabled@ represents meta information our system has regards
    -- to if particular trace functions are enabled and if there is a
    -- general debug level we can print out instead.
    tEnabled :: HashMap.T NameSymbol.T MetaInfo,
    -- | @tDebugLevel@ represents the debug level we care about
    tDebugLevel :: Maybe Natural
  }
  deriving (Show)

type Log = [Stack]

data StackChain
  = Empty
  | StackChain
      { parent :: StackChain,
        currentStack :: Stack
      }
  deriving (Show)

-- | @Stack@ represents a stack-trace for a particular function
data Stack = Stack
  { -- | @stackName@ represents the name of the stackd function
    stackName :: NameSymbol.T,
    -- | @stackdStart@ represents the incoming function arguments
    stackStart :: [T.Text],
    -- | @stackBetween@ represents the Traces that have happened
    -- between this call and the end of the call
    stackBetween :: Log,
    -- | @stackOutput@ represents the output result. Maybe as we could
    -- stop before it's finished!
    stackOutput :: Maybe T.Text
  }
  deriving (Show)

data MetaInfo = MetaInfo
  { metaInfoEnable :: Enable,
    metaInfoLevel :: Natural
  }
  deriving (Show)

data Enable
  = -- | @Enabled@ represents a trace enabled function
    Enabled
  | -- | @Disabled@ represents that the function is disabled
    Disabled
  | -- | @DisableRecursive@ represents that we shouldn't trace any
    -- Enabled functions inside the scope of the Trace
    DisableRecursive
  deriving (Show)

Lens.makeLensesWith Lens.camelCaseFields ''Stack
Lens.makeLensesWith Lens.camelCaseFields ''T
Lens.makeLensesWith Lens.camelCaseFields ''MetaInfo

--------------------------------------------------------------------------------
-- Core API
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Helper Functionality
--------------------------------------------------------------------------------

startScope :: T -> Stack -> T
startScope t stack =
  over current (`StackChain` stack) t

finishScope :: T -> T
finishScope t =
  case t ^. current of
    Empty -> t
    StackChain Empty stack ->
      t |> over traces (stack :) |> set current Empty
    StackChain (StackChain grandParent parent) stack ->
      set current (StackChain grandParent (consLog stack parent)) t

consLog :: HasBetween t [a] => a -> t -> t
consLog currentStack =
  over between (currentStack :)
