module CPSTranslation where

import qualified Juvix.Desugar as Desugar
import Juvix.Library hiding (head)
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types as Types
import qualified Juvix.Pipeline.ToSexp as ToSexp
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Witch.CPSTranslation as Conv
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, head)

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
        "print effect program desugars correctly"
        (expected T.@=? form),
      T.testCase
        "print effect program translates correctly"
        (form T.@=? expected)
    ]
  where
    sexp =
      fmap
        Parser.parse
        [ "handler printer =           "
            <> "     let print = printLn     "
            <> "     let pure x = toString x "
            <> " end                         ",
          "let prog a = print a; pure a ",
          "let foo = prog via printer   "
        ]

    prog = sexp |> sequence

    form =
      sexp
        |> fmap singleEleErr
        |> desugar

    expected =
      fmap
        Sexp.parse
        [ "(:lethandler printer (:defret (x) (toString x)) (:defop print () printLn))",
          "(:defsig-match prog () ((a) (:do (:do-op print (a)) (:do-pure a))))",
          "(:defsig-match foo () (() (:via printer prog)))"
        ]
        |> sequence

complexProg :: T.TestTree
complexProg =
  T.testGroup
    "complex effect handling"
    [ T.testCase
        "multiple effects"
        (expected T.@=? form)
    ]
  where
    form =
      fmap
        Sexp.parse
        [""]
        |> sequence
    expected =
      fmap
        Sexp.parse
        [""]
        |> sequence

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

desugar :: Monad f => [f Sexp.T] -> f [Sexp.T]
desugar = fmap Desugar.op . sequence

singleEleErr :: Functor f => f (Types.Header Types.TopLevel) -> f Sexp.T
singleEleErr = fmap (ToSexp.transTopLevel . head . noHeaderErr)

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = error "improper form"
