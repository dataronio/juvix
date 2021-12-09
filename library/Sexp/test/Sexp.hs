module Sexp (top) where

import qualified Data.IORef as IORef
import qualified Data.Set as Set
import Juvix.Library
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error)

top :: T.TestTree
top =
  T.testGroup
    "sexp pass tests:"
    [ lastWorksAsExpected,
      foldrWorksAsExpted,
      mapPredStarWorksAsExpted,
      mapPredStarWorksAsExpted2,
      mapPredStarWorksAsExpted3,
      listWorksAsExpected,
      listStarWorksAsExpected
    ]

--------------------------------------------------------------------------------
-- Tests not exported
--------------------------------------------------------------------------------

lastWorksAsExpected :: T.TestTree
lastWorksAsExpected =
  T.testGroup
    "last works as expected"
    [ T.testCase
        "last on Cons"
        (fmap Sexp.last (Sexp.parse "(1 2 3 4)") T.@=? Sexp.parse "4"),
      T.testCase
        "last on atom"
        (fmap Sexp.last (Sexp.parse "4") T.@=? Sexp.parse "4"),
      T.testCase
        "last on Nil"
        (fmap Sexp.last (Sexp.parse "()") T.@=? Sexp.parse "()")
    ]

foldrWorksAsExpted :: T.TestTree
foldrWorksAsExpted =
  T.testGroup
    "foldr works as epxected"
    [ T.testCase
        "foldr on (1 2 3 4 5) properly adds"
        (Sexp.foldr (\(Sexp.Atom (Sexp.N n Nothing)) acc -> n + acc) 0 ns T.@=? 15)
    ]
  where
    Right ns = Sexp.parse "(1 2 3 4 5)"

mapPredStarWorksAsExpted :: T.TestTree
mapPredStarWorksAsExpted =
  T.testGroup
    "mapPredStar works as Exptected"
    [ T.testCase
        "mapPredStar properly recurses and does not change the non car position"
        ( Sexp.mapPredStar nest (== "if") (Sexp.Cons (Sexp.atom ":if") . Sexp.cdr)
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse "(if x y (if if l))"
    Right expectedNest =
      Sexp.parse "(:if x y (:if if l))"

mapPredStarWorksAsExpted2 :: T.TestTree
mapPredStarWorksAsExpted2 =
  T.testGroup
    "mapPredStar works as Exptected"
    [ T.testCase
        "mapPredStar properly searches"
        ( Sexp.mapPredStar nest (== "if") (Sexp.Cons (Sexp.atom ":if") . Sexp.cdr)
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse "(g x y (f z l))"
    Right expectedNest =
      Sexp.parse "(g x y (f z l))"

mapPredStarWorksAsExpted3 :: T.TestTree
mapPredStarWorksAsExpted3 =
  T.testGroup
    "mapPredStar works as Exptected"
    [ T.testCase
        "mapPredStar can deal with bigger sexp"
        ( Sexp.mapPredStar
            nest
            (== ":defop")
            (\sexp -> Sexp.Cons (Sexp.atom ":op") (Sexp.cdr sexp))
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse
        ( "(:defmodule Printer ()                     "
            <> "   (:defhandler printer                     "
            <> "    ((:defop print () printLn)              "
            <> "     (:defop pure (x) (toString x))))       "
            <> "   (:defun prog (a)                         "
            <> "      (:do                                  "
            <> "          (:do-body (:do-op print (a)))     "
            <> "          (:do-body (:do-pure a))))         "
            <> "   (:defun foo ()                           "
            <> "      (:via printer prog)))))"
        )
    Right expectedNest =
      Sexp.parse
        ( "(:defmodule Printer ()                       "
            <> "   (:defhandler printer                     "
            <> "    ((:op print () printLn)                 "
            <> "     (:op pure (x) (toString x))))          "
            <> "   (:defun prog (a)                         "
            <> "      (:do                                  "
            <> "          (:do-body (:do-op print (a)))     "
            <> "          (:do-body (:do-pure a))))         "
            <> "   (:defun foo ()                           "
            <> "      (:via printer prog)))))"
        )

listWorksAsExpected :: T.TestTree
listWorksAsExpected =
  T.testGroup
    "list works as epctected"
    [ T.testCase
        "list on a term is correct"
        (Sexp.parse "(1 2 3)" T.@=? Right manualList)
    ]
  where
    manualList =
      Sexp.list [Sexp.number 1, Sexp.number 2, Sexp.number 3]

listStarWorksAsExpected :: T.TestTree
listStarWorksAsExpected =
  T.testGroup
    "list* works as epctected"
    [ T.testCase
        "list* on a list properly removes the last list"
        (Sexp.parse "(1 2 3)" T.@=? Right manualList)
    ]
  where
    manualList =
      Sexp.listStar [Sexp.number 1, Sexp.number 2, Sexp.list [Sexp.number 3]]
