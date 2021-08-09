module Convert where

import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Juvix.ANF.Convert as Conv
import qualified Juvix.Sexp as Sexp

top :: T.TestTree
top =
  T.testGroup
    "CPS conversion passes tests:"
    [ simpleProg,
      complexProg,
    ]

simpleProg :: T.TestTree
simpleProg =
  T.testGroup
    "simple effect handling"
    [ T.testCase
        "print effect"
        (expected T.@=? fmap Conv.convert form)
    ]
  where
    form =
      Sexp.parse
        ""
    expected =
      Sexp.parse
        ""

complexProg :: T.TestTree
compleProg =
  T.testGroup
    "complex effect handling"
    [ T.testCase
        "multiple effects"
        (expected T.@=? fmap Conv.convert form)
    ]
  where
    form =
      Sexp.parse
        ""
    expected =
      Sexp.parse
        ""
