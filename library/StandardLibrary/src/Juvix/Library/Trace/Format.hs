module Juvix.Library.Trace.Format where

import Control.Lens ((^.))
import qualified Data.Text as Text
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Trace.Types

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

currentStackChain :: StackChain -> (Integer -> Integer) -> [Char]
currentStackChain chain indentationIncrement =
  chain
    |> stackChainToList
    -- Let's start with the parent
    |> foldr f ("", 0)
    |> fst
  where
    f current (formattedString, indentation) =
      ( formattedString
          <> spacing indentation
          <> functionCall (current ^. name) (current ^. start)
          <> "\n",
        indentationIncrement indentation
      )

fullTrace :: T -> (Integer -> Integer) -> [Char]
fullTrace t indentationIncrement =
  traceLog t 0 indentationIncrement (t ^. traces)

traceLog ::
  T -> Integer -> (Integer -> Integer) -> [Stack] -> [Char]
traceLog t level indentationIncrement stacks =
  fmap (traceStack t level indentationIncrement) stacks
    |> filter (/= mempty)
    |> reverse
    |> intersperse "\n"
    |> fold
    |> (<> "\n")

traceStack :: T -> Integer -> (Integer -> Integer) -> Stack -> [Char]
traceStack t currentLevel indentationIncrement stack =
  case HashMap.lookup (stack ^. name) (t ^. enabled) of
    Just metaInfo
      | (t ^. debugLevel) >= Just (metaInfo ^. level) ->
        callFull
    Just metaInfo ->
      case metaInfo ^. enable of
        Disabled ->
          callIgnoreCurrent
        Enabled ->
          callFull
        DisableRecursive ->
          ""
    Nothing
      | (t ^. debugLevel) >= Just maxTrace ->
        callFull
    Nothing ->
      callIgnoreCurrent
  where
    callFull =
      -- Bad case, make it better as we may end up
      -- ·· (Prelude.add2 12.0)
      -- ·· (Prelude.add2 12.0) ↦ 14.0
      -- if we filter our between list
      case stack ^. between of
        [] ->
          spacing currentLevel <> returnResult stack
        _ : _ ->
          let inc = indentationIncrement
           in spacing currentLevel
                <> functionCall (stack ^. name) (stack ^. start)
                <> "\n"
                <> traceLog t (inc currentLevel) inc (stack ^. between)
                <> spacing currentLevel
                <> returnResult stack
    callIgnoreCurrent =
      traceLog t currentLevel indentationIncrement (stack ^. between)

--------------------------------------------------------------------------------
-- Formatting Helpers
--------------------------------------------------------------------------------

customSpacing :: Char -> Int -> [Char]
customSpacing = flip replicate

spacing :: Integer -> [Char]
spacing 0 = ""
spacing n = customSpacing '·' (fromInteger n) <> " "

functionCall :: NameSymbol.T -> [Text] -> [Char]
functionCall f args =
  "("
    <> unintern (NameSymbol.toSymbol f)
    <> space
    <> foldMap Text.unpack (intersperse " " args)
    <> ")"
  where
    space =
      case args of
        [] -> ""
        _ : _ -> " "

returnResult :: Stack -> [Char]
returnResult stack =
  let res = maybe "No Return" Text.unpack (stack ^. output)
   in functionCall (stack ^. name) (stack ^. start) <> " ↦ " <> res

--------------------------------------------------------------------------------
-- Utility Helpers
--------------------------------------------------------------------------------

stackChainToList :: StackChain -> [Stack]
stackChainToList Empty = []
stackChainToList StackChain {parent = cdr, currentStack = car} =
  car : stackChainToList cdr
