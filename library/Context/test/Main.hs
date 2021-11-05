module Main where

import qualified Context
import qualified Test.Tasty as T
import Juvix.Library.Fetch (loadStdLibs)

allCheckeddTests :: T.TestTree
allCheckeddTests =
  T.testGroup
    "Context tests"
    [Context.top]

main = do
  loadStdLibs
  T.defaultMain allCheckeddTests
