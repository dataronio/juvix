module Main where

import Juvix.Library (IO)
import qualified Test.Tasty as T
import Juvix.Library.StdLib (loadStdLibs)

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    []

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
