module Main where

import Juvix.Library
import Juvix.Library.Fetch (loadStdLibs)

main :: IO ()
main = do
  loadStdLibs
  pure ()
