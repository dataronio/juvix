module Test.Contextify.Binders (top) where

import qualified Juvix.Contextify.Binders as Bind
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--   + (:let-match f ((args-match-11 … args-match-1n) body-1
--                    (args-match-21 … args-match-2n) body-2
--                    …
--                    (args-match-n1 … args-match-nn) body-n)
--        rest)

data Infix
  = Infix
      { infixOp :: NameSymbol.T,
        infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
        infixInf :: Infix
      }
  | InfixNoMore
      { infixOp :: NameSymbol.T,
        infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
        infixRt :: (Sexp.B (Bind.BinderPlus Infix))
      }
  deriving (Show, Generic, Eq)

infixRename :: Sexp.Options
infixRename =
  Sexp.changeName
    (Sexp.defaultOptions @Infix)
    (Map.fromList [("InfixNoMore", ":infix")])

instance Sexp.DefaultOptions Infix

instance Sexp.Serialize Infix where
  serialize = Sexp.serializeOpt infixRename
  deserialize = Sexp.deserializeOpt infixRename

top :: T.TestTree
top =
  T.testGroup
    "Testing proper serialization and deserialization behavior"
    [ T.testCase "testing let match" $ do
        let Right xs =
              Sexp.parse
                "(:let-match f \
                \    ((args-match-11 … args-match-1n) body-1 \
                \     (args-match-21 … args-match-2n) body-2) \
                \  rest)"
            Just deserialized = Sexp.deserialize @(Bind.BinderPlus ()) xs
            expected =
              Bind.LetMatch
                <$> pure "f"
                <*> argumentBinder
                <*> parseBinder "rest"
        expected T.@=? Just deserialized,
      T.testCase "testing nested match" $ do
        let Right xs =
              Sexp.parse
                "(:let-match f \
                \    ((args-match-11 … args-match-1n) body-1 \
                \     (args-match-21 … args-match-2n) body-2) \
                \  (case x ((a) (:infix + a b)) ((b) (:infix + a b))))"
            Just deserialized = Sexp.deserialize @(Bind.BinderPlus ()) xs
            expected =
              Bind.LetMatch
                <$> pure "f"
                <*> argumentBinder
                <*> ( parseMaybe "(case x ((a) (:infix + a b)) ((b) (:infix + a b)))"
                        >>= fmap Sexp.primOp . Sexp.deserialize @(Bind.BinderPlus ())
                    )
        expected T.@=? Just deserialized,
      T.testCase "users matches win out" $ do
        let Right xs =
              Sexp.parse
                "(:let-match f \
                \    ((args-match-11 … args-match-1n) body-1 \
                \     (args-match-21 … args-match-2n) body-2) \
                \  (case x ((a) (:infix + a b)) ((b) (:infix + a b))))"
            Just deserialized = Sexp.deserialize @(Bind.BinderPlus (Bind.Case ())) xs
            expected =
              Bind.LetMatch
                <$> pure "f"
                <*> argumentBinder
                <*> ( parseMaybe "(case x ((a) (:infix + a b)) ((b) (:infix + a b)))"
                        >>= fmap (Sexp.primOp . Bind.Other) . Sexp.deserialize @(Bind.Case ())
                    )
        expected T.@=? Just deserialized
    ]

-- Sexp.serialize
--   @[ArgBody ()]
--    [ ArgBody (Sexp.List [Sexp.atom "a", Sexp.atom "b"]) (Sexp.List [Sexp.number 3])
--    , ArgBody (Sexp.List [])                             (Sexp.List [Sexp.number 4])]
-- > (("a" "b") (3) () (4))
argumentBinder :: Sexp.Serialize a => Maybe [Bind.ArgBody a]
argumentBinder =
  ( parseMaybe
      "((args-match-11 … args-match-1n) body-1 \
      \ (args-match-21 … args-match-2n) body-2)"
      >>= Sexp.deserialize
  )

parseMaybe :: ByteString -> Maybe Sexp.T
parseMaybe thing =
  case Sexp.parse thing of
    Right xs -> Just xs
    Left _ -> Nothing

parseBinder :: ByteString -> Maybe (Sexp.B (Bind.BinderPlus ()))
parseBinder thing =
  parseMaybe thing >>| Sexp.partiallyDeserialize @(Bind.BinderPlus ())

parseNested :: ByteString -> Maybe (Sexp.B (Bind.BinderPlus (Bind.Case ())))
parseNested thing =
  parseMaybe thing >>| Sexp.partiallyDeserialize @(Bind.BinderPlus (Bind.Case ()))
