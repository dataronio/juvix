import qualified FromFrontend
import Juvix.Library
import Juvix.Library.Fetch (loadStdLibs)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain FromFrontend.top
