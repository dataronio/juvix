module Main where

import Juvix.Library (IO)
import qualified Test.Tasty as T
import qualified Convert as Conv

effectHandlerTests :: T.TestTree
effectHandlerTests =
  T.testGroup
    "CPS translation"
    [ Conv.top ]

anfTransTests :: T.TestTree
anfTransTests =
  T.testGroup
    "ANF translation"
    []

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ effectHandlerTests,
      anfTransTests
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
