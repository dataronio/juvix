module Main where

import Juvix.Library (IO)
import Juvix.Library.Fetch (loadStdLibs)
import Parser (allParserTests)
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [allParserTests]

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
