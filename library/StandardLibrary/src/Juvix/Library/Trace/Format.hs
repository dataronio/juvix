module Juvix.Library.Trace.Format where

import Control.Lens (over, set, (^.))
import qualified Data.Text as Text
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Trace.Types

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

currentStackChain :: StackChain -> (Int -> Int) -> ([Char], Int)
currentStackChain chain indentationIncrement =
  chain
    |> stackChainToList
    -- Let's start with the parent
    |> foldr f ("", 0)
  where
    f current (formattedString, indentation) =
      ( formattedString
          <> spacing indentation
          <> functionCall (current ^. name) (current ^. start)
          <> "\n",
        indentationIncrement indentation
      )

--------------------------------------------------------------------------------
-- Formatting Helpers
--------------------------------------------------------------------------------

customSpacing :: Char -> Int -> [Char]
customSpacing = flip replicate

spacing :: Int -> [Char]
spacing = customSpacing '·'

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
          _: _ -> " "

--------------------------------------------------------------------------------
-- Utility Helpers
--------------------------------------------------------------------------------

stackChainToList :: StackChain -> [Stack]
stackChainToList Empty = []
stackChainToList StackChain {parent = cdr, currentStack = car} =
  car : stackChainToList cdr
