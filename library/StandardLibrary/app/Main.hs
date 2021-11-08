module Main where

import Juvix.Library (IO)
import Juvix.Library.Fetch (downloadStdLibs)

main :: IO ()
main = downloadStdLibs
