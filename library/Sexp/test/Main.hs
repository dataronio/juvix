module Main where

import Juvix.Library
import qualified Sexp
import qualified Sexp.Parser
import qualified Sexp.SimplifiedPasses
import qualified Test.Tasty as T
import Juvix.Library.StdLib (loadStdLibs)
allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ Sexp.top,
      Sexp.Parser.top,
      Sexp.SimplifiedPasses.top
    ]

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
