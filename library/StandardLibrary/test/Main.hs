module Main where

import qualified Golden
import Juvix.Library
import qualified Juvix.Library.Test.Golden as TG
import qualified NameSymb
import qualified Pretty
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ NameSymb.top,
      Pretty.top
    ]

main :: IO ()
main = do
  TG.runAll
    [ T.defaultMain allCheckedTests,
      TG.defaultMainFail Golden.absurdTestAll,
      TG.defaultMainFail Golden.absurdTestSome
    ]
