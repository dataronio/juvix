module HR.Serialize where

import Juvix.Core.HR as HR
import qualified Juvix.Core.HR.Sexp as Sexp
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  -- TODO test syntax highlighting stuff
  T.testGroup
    "HR pretty printing"
    [ T.testCase "app2" $
        app2
          |> Sexp.serializeElim
          |> (Sexp.deserializeElim :: Sexp.T -> Maybe (HR.Elim () ()))
          |> isJust
          |> (T.@?= True),
      T.testCase "pair" $
        three
          |> Sexp.serialize
          |> (Sexp.deserialize :: Sexp.T -> Maybe (HR.Term () ()))
          |> isJust
          |> (T.@?= True),
      T.testCase "pi" $
        two'
          |> Sexp.serialize
          |> (Sexp.deserialize :: Sexp.T -> Maybe (HR.Term () ()))
          |> isJust
          |> (T.@?= True)
    ]

cxy = Elim cxy'

cxy' = "C" `AppE` "x" `AppE` "y"

xAB =
  Pi SAny "x" "A" $
    Elim $
      "B" `App` "x"

xAyBC :: HR.Term () ()
xAyBC =
  Pi SAny "x" (var "A") $
    Pi SAny "y" (var "B") $
      Elim $
        HR.Var "C" `App` var "x" `App` var "y"

xAyBC' =
  Pi SAny "x" "A" $
    Sig SAny "y" "B" $
      cxy

fxy' :: HR.Elim () ()
fxy' = HR.Var "f" `AppE` HR.Var "x" `AppE` HR.Var "y"

gxy' :: HR.Elim () ()
gxy' = HR.Var "g" `AppE` HR.Var "x" `AppE` HR.Var "y" `App` HR.Prim ()

fxy :: HR.Term () ()
fxy = Elim fxy'

gxy :: HR.Term () ()
gxy = Elim gxy'

two' :: HR.Term () ()
two' = xAyBC `Pair` xAyBC

three :: HR.Term () ()
three = fxy `Pair` (gxy `Pair` fxy)

app1 :: HR.Elim () ()
app1 =
  HR.Var "zipApply3"
    `App` (var "f" `Pair` var "g" `Pair` var "h")
    `App` (var "x" `Pair` (var "y" `Pair` var "z"))

app2 :: HR.Elim () ()
app2 = HR.Var "merge" `AppE` app1 `AppE` app1

var = HR.Elim . HR.Var

pattern AppE f e = App f (Elim e)
