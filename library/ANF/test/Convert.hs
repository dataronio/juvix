module Convert where

import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (head)
import qualified Juvix.Pipeline.ToSexp as ToSexp
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, head)
import qualified Juvix.ANF.Convert as Conv

top :: T.TestTree
top =
  T.testGroup
    "CPS conversion passes tests:"
    [ simpleProg,
      complexProg
    ]

simpleProg :: T.TestTree
simpleProg =
  T.testGroup
    "simple effect handling"
    [ T.testCase
        "print effect"
        (expected T.@=? fmap Conv.convertEffects form)
    ]
  where
    form =
      Parser.parse
        " handler printer =                 \
        \   let print = IO.print            \
        \   let pure x  = toString x        \
        \                                   \
        \ let prog a =                      \
        \   print a;                        \
        \   pure a                          \
        \                                   \
        \ let foo = prog via printer"
        |> singleEleErr
    expected =
      Sexp.parse
        ""

complexProg :: T.TestTree
complexProg =
  T.testGroup
    "complex effect handling"
    [ T.testCase
        "multiple effects"
        (expected T.@=? fmap Conv.convertEffects form)
    ]
  where
    form =
      Sexp.parse
        ""
    expected =
      Sexp.parse
        ""

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

singleEleErr :: Functor f => f (Types.Header Types.TopLevel) -> f Sexp.T
singleEleErr = fmap (ToSexp.transTopLevel . head . noHeaderErr)

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = error "improper form"
