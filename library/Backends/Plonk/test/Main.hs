module Main where

import Juvix.Library
import qualified Test.Compiler as Compiler
import qualified Test.Golden as Golden
import qualified Test.Tasty as T
import Juvix.Library.Fetch (loadStdLibs)

main :: IO ()
main = do
  loadStdLibs
  compilerTests <- Compiler.top
  goldenTests <- Golden.top
  T.defaultMain $
    T.testGroup "Plonk tests" [compilerTests, goldenTests]
