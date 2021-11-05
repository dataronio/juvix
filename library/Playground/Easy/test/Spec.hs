import qualified FromFrontend
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Juvix.Library.Fetch (loadStdLibs)

main :: IO ()
main = do
    loadStdLibs
    T.defaultMain FromFrontend.top
