module Main where

import qualified Context
import Juvix.Library.Fetch (loadStdLibs)
import qualified Test.Tasty as T

allCheckeddTests :: T.TestTree
allCheckeddTests =
  T.testGroup
    "Context tests"
    [Context.top]

main = do
  loadStdLibs
  T.defaultMain allCheckeddTests
