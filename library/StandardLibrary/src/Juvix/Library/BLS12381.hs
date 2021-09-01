{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Juvix.Library.BLS12381 (Fr, Galois.fromP, Galois.toP) where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Data.Curve.Weierstrass.BLS12381 (Fr)
import qualified Data.Field.Galois as Galois (fromP, toP)
import qualified Data.Scientific as S
import Juvix.Library
import Text.Read (Lexeme (..), Read (..), lexP, parens, pfail, step)
import qualified Text.Read.Lex as Read.Lex

------------------------------------------------------------------------------

instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left d -> panic $ "Can't parse floating:" <> show (d :: Double)
    Right f -> pure $ Galois.toP f
  parseJSON j = panic $ "Can't parse non-number:" <> show j

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (Galois.fromP f) 0

instance Read Fr where
  readPrec = parens $ do
    Ident "P" <- step lexP
    Punc "(" <- step lexP
    Number n <- step lexP
    Punc "`" <- step lexP
    Ident "modulo" <- step lexP
    Punc "`" <- step lexP
    Number _ <- step lexP
    Punc ")" <- step lexP
    case Read.Lex.numberToInteger n of
      Just i -> pure $ Galois.toP i
      Nothing -> pfail

deriving instance Bits Fr
