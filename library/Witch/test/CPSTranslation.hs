module CPSTranslation where

import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (head)
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
        [ "(:lethandler printer (:ops (:defop print () printLn)) (:defret (x) (toString x)))",
          "(:defsig-match prog () ((a) (:do (:do-body (:do-op print (a))) (:do-body (:do-pure a)))))",
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
