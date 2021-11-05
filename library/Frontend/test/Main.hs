module Main where

import Juvix.Library (IO)
import Parser (allParserTests)
import qualified Test.Tasty as T
import Juvix.Library.Fetch (loadStdLibs)
allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [allParserTests]

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
