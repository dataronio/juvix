module Main where

import Juvix.Library
import Juvix.Library.Fetch (loadStdLibs)
import qualified Test.Golden as Golden
import qualified Test.Parameterization as Parameterization
import qualified Test.Tasty as T

main :: IO ()
main = do
  loadStdLibs
  goldenTests <- Golden.top
  T.defaultMain $
    T.testGroup "LLVM tests" [goldenTests, Parameterization.top]
