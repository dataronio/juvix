import qualified FromFrontend
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Juvix.Library.StdLib (loadStdLibs)

main :: IO ()
main = do
    loadStdLibs
    T.defaultMain FromFrontend.top
