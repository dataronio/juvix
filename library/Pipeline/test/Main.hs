module Main where

import Juvix.Library (IO)
import qualified Test.RecGroups as RecGroups
import qualified Test.Tasty as T
import qualified Test.ToSexp as ToSexp
import Juvix.Library.Fetch (loadStdLibs)

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [RecGroups.top, ToSexp.top]

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
