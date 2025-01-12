module Test.Desugar.Sexp (top) where

import qualified Juvix.Desugar as Desugar
import qualified Juvix.Desugar.Passes as Desugar
import Juvix.Library hiding (head)
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types as Types
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Translate.Pipeline.TopLevel as ToSexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, head)

top :: T.TestTree
top =
  T.testGroup
    "sexp desugar passes tests:"
    [ condWorksAsExpected,
      ifWorksAsExpected,
      letWorksAsExpected,
      defunWorksAsExpcted,
      sigWorksAsExpcted,
      recordsWorkAsExpected,
      modulesWorkAsExpected,
      modLetWorkAsExpected,
      handlerWorkAsExpected,
      handlerTest2
    ]

condWorksAsExpected :: T.TestTree
condWorksAsExpected =
  T.testGroup
    "cond desugar tests"
    [ T.testCase
        "recursive conds work"
        (expected T.@=? fmap Desugar.condTransform form)
    ]
  where
    form =
      Sexp.parse
        "(:defun foo (x)\
        \   (:cond ((:infix + x 3) 2) \
        \          ((bar true)     (:paren (:cond ((bar false) 3) (else 5))))\
        \          (else           3)))"
    expected =
      Sexp.parse
        "(:defun foo (x) \
        \   (if (:infix + x 3)\
        \   2\
        \   (if (bar true)\
        \       (:paren (if (bar false) 3 (if else 5))) \
        \       (if else 3))))"

ifWorksAsExpected :: T.TestTree
ifWorksAsExpected =
  T.testGroup
    "if desugar tests"
    [ T.testCase
        "recursive ifs work"
        (expected T.@=? fmap Desugar.ifTransform form)
    ]
  where
    form =
      Sexp.parse
        "(:defun foo (x) \
        \   (if (:infix + x 3)\
        \   2\
        \   (if (bar true)\
        \       (:paren (if (bar false) 3 (if else 5)))\
        \       (if else 3))))"
    expected =
      Sexp.parse
        "(:defun foo (x) \
        \  (case (:infix + x 3)\
        \     ((True) 2)\
        \     ((False) (case (bar true)\
        \                 ((True) (:paren\
        \                           (case (bar false)\
        \                             ((True) 3)\
        \                             ((False) (case else ((True) 5))))))\
        \                 ((False) (case else ((True) 3)))))))"

-- TODO ∷ Add another let form which aren't combined
letWorksAsExpected :: T.TestTree
letWorksAsExpected =
  T.testGroup
    "let desugar tests"
    [ T.testCase
        "multiple lets work"
        (expected T.@=? fmap Desugar.multipleTransLet form)
    ]
  where
    form =
      Sexp.parse
        "(:defun foo () \
        \    (let f ((Cons x y) b) (:infix + x (:infix + y b))\
        \       (let f (Nil b) b\
        \          (foo (:paren (Cons 1 2)) 3))))"
    expected =
      Sexp.parse
        "(:defun foo ()\
        \    (:let-match f (((Cons x y) b) (:infix + x (:infix + y b))\
        \                   (Nil b)        b)\
        \       (foo (:paren (Cons 1 2)) 3)))"

-- TODO ∷ Add another let form which aren't combined
defunWorksAsExpcted :: T.TestTree
defunWorksAsExpcted =
  T.testGroup
    "defun desugar tests"
    [ T.testCase
        "combining two functions work"
        (expected T.@=? fmap Desugar.multipleTransDefun form),
      T.testCase
        "combining two seperate functions is id"
        (expectedNon T.@=? fmap Desugar.multipleTransDefun formNon)
    ]
  where
    form =
      traverse
        Sexp.parse
        ["(:defun foo ((Cons a b) b) 3)", "(:defun foo ((Nil) b) 1)"]
    expected =
      traverse
        Sexp.parse
        ["(:defun-match foo (((Cons a b) b) 3) (((Nil) b) 1))"]
    formNon =
      traverse
        Sexp.parse
        ["(:defun foo ((Cons a b) b) 3)", "(:defun bar ((Nil) b) 1)"]
    expectedNon =
      traverse
        Sexp.parse
        ["(:defun-match foo (((Cons a b) b) 3))", "(:defun-match bar (((Nil) b) 1))"]

sigWorksAsExpcted :: T.TestTree
sigWorksAsExpcted =
  T.testGroup
    "sig desugar tests"
    [ T.testCase
        "combining a sig and function work"
        (expected T.@=? fmap (Desugar.combineSig . Desugar.multipleTransDefun) form),
      T.testCase
        "combining differ drops"
        (expectedNon T.@=? fmap (Desugar.combineSig . Desugar.multipleTransDefun) formNon)
    ]
  where
    form =
      traverse
        Sexp.parse
        ["(:defsig foo (:infix -> int int))", "(:defun foo (i) (:infix + i 1))"]
    expected =
      traverse
        Sexp.parse
        ["(:defsig-match foo (:infix -> int int) ((i) (:infix + i 1)))"]
    formNon =
      traverse
        Sexp.parse
        ["(:defsig bar (:infix -> int int))", "(:defun foo (i) (:infix + i 1))"]
    -- should we have the arguments be a bit variable if none is given!?
    expectedNon =
      traverse
        Sexp.parse
        ["(:defsig-match foo () ((i) (:infix + i 1)))"]

recordsWorkAsExpected :: T.TestTree
recordsWorkAsExpected =
  T.testGroup
    "record desugar tests"
    [ T.testCase
        "record expnasion expansion work"
        (expected T.@=? fmap Desugar.removePunnedRecords form)
    ]
  where
    form =
      Sexp.parse
        "(:defun foo (a b) (:record (a) (b 2)))"
    expected =
      Sexp.parse
        "(:defun foo (a b) (:record-no-pun a a b 2))"

modulesWorkAsExpected :: T.TestTree
modulesWorkAsExpected =
  T.testGroup
    "module desugar tests"
    [ T.testCase
        "basic expansion expansion work"
        (expected T.@=? fmap Desugar.moduleTransform form),
      T.testCase
        "Type expansion works"
        (expectedComplicated T.@=? fmap Desugar.moduleTransform formComplicated)
    ]
  where
    form =
      Sexp.parse
        "(:defmodule f ()\
        \    (:defmodule b ()\
        \       (:defun a () 2))\
        \    (type bazzz () (Foo (:record-d a int b int)) (Snash))\
        \    (:defun fi () 3))"
    expected =
      Sexp.parse
        "(:defun f ()\
        \   (:let-mod b () ((:defun a () 2))\
        \      (:let-type bazzz () ((Foo (:record-d a int b int))\
        \                           (Snash))\
        \         (let fi () 3\
        \            (:record (b) (bazzz) (fi))))))"

    formComplicated =
      Sexp.parse
        "(:defmodule f ()\
        \    (:defmodule b ()\
        \       (:defun a () 2))\
        \    (type bazzz () (Foo (:record-d a int b int)) (Snash))\
        \    (type (foo :type (:infix -> typ (:infix -> typ typ))) (x y z)\
        \       (Foo a b c))\
        \    (:defun fi () 3))"
    expectedComplicated =
      Sexp.parse
        "(:defun f ()\
        \   (:let-mod b () ((:defun a () 2))\
        \      (:let-type bazzz () ((Foo (:record-d a int b int))\
        \                           (Snash))\
        \ (:let-type (foo :type (:infix -> typ (:infix -> typ typ))) \
        \            (x y z) ((Foo a b c))\
        \    (let fi () 3 (:record (b) (bazzz) (fi) (foo)))))))"

modLetWorkAsExpected :: T.TestTree
modLetWorkAsExpected =
  T.testGroup
    "module let desugar tests"
    [ T.testCase
        "basic module expansion work"
        (expected T.@=? fmap Desugar.moduleLetTransform form)
    ]
  where
    form =
      Sexp.parse
        "(:defun foo ()\
        \   (:let-mod foo () ((:defun bar () 3)\
        \                     (type foo () (XTZ)))\
        \     foo))"
    expected =
      Sexp.parse
        "(:defun foo ()\
        \   (let foo ()\
        \         (let bar () 3 \
        \            (:let-type foo () ((XTZ))\
        \               (:record (bar) (foo))))\
        \     foo))"

handlerWorkAsExpected :: T.TestTree
handlerWorkAsExpected =
  T.testGroup
    "handler let desugar tests"
    [ T.testCase
        "defhandler -desugar-> lethandler"
        (expected T.@=? fmap Desugar.handlerTransform form)
    ]
  where
    form =
      Sexp.parse
        ( "(:defhandler printer                     "
            <> "  ((:defop print () printLn)             "
            <> "   (:defop pure (x) (toString x))))      "
        )
    expected =
      Sexp.parse
        ( "(:lethandler printer                     "
            <> "  (:defret (x) (toString x))            "
            <> "  (:defop print () printLn))       "
        )

handlerTest2 :: T.TestTree
handlerTest2 =
  T.testGroup
    "handler from syntax to desugared sexp"
    [ T.testCase "basic" (basic T.@=? basicExpected)
    ]
  where
    basic =
      Parser.parse
        "handler print = let print x = print x let pure x = toString x end"
        |> singleEleErr
        |> desugar
    basicExpected =
      Sexp.parse
        "(:lethandler print (:defret (x) (toString x)) (:defop print (x) (print x)))"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

desugar :: Functor f => f Sexp.T -> f Sexp.T
desugar = fmap head . fmap Desugar.op . fmap pure

singleEleErr :: Functor f => f (Types.Header Types.TopLevel) -> f Sexp.T
singleEleErr = fmap (ToSexp.transTopLevel . head . noHeaderErr)

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = error "improper form"
