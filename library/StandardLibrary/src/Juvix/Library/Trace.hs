{-# LANGUAGE TemplateHaskell #-}

-- | The Trace library represents the ability to properly trace code
-- throughout Haskell.
--
-- - The structure requires your code to exist in the =Trace.Eff=
--   effect.
module Juvix.Library.Trace (module Juvix.Library.Trace, module Juvix.Library.Trace.Types) where

import Juvix.Library.Trace.Types
import Control.Lens (over, set, (^.))
import qualified Data.Text as T
import qualified Juvix.Library.Trace.Format as Format
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol

--------------------------------------------------------------------------------
-- Core API
--------------------------------------------------------------------------------

withScope :: (Eff m, Show b) => NameSymbol.T -> [Text] -> m b -> m b
withScope name args f = do
  let newStack =
        Stack
          { stackName = name,
            stackStart = args,
            stackBetween = [],
            stackOutput = Nothing
          }
  modify @"trace" (`startScope` newStack)
  ret <- f
  modify @"trace" (finishScope . (`registerOutput` show ret))
  pure ret

format = undefined

-- | @break@ dumps the current Stack Trace, Ignoring any disabling behavior
break :: (Eff m, MonadIO m) => m ()
break = do
  trace <- get @"trace"
  putStrLn (Format.currentStackChain (trace ^. current) (const 1))


--------------------------------------------------------------------------------
-- Helper Functionality
--------------------------------------------------------------------------------

registerOutput :: T -> Text -> T
registerOutput t result =
  case t ^. current of
    Empty -> t
    StackChain a stack ->
      set current (StackChain a (set output (Just result) stack)) t

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
