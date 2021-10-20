module Main where

import Juvix.Library (IO)
import qualified Test.Tasty as T
import qualified Test.ToSexp as ToSexp

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    []

main :: IO ()
main = T.defaultMain allCheckedTests
