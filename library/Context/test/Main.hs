module Main where

import qualified Context
import qualified Test.Tasty as T
import Juvix.Library.StdLib (loadStdLibs)

allCheckeddTests :: T.TestTree
allCheckeddTests =
  T.testGroup
    "Context tests"
    [Context.top]

main = do
  loadStdLibs
  T.defaultMain allCheckeddTests
