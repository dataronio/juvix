{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Juvix.Library.BLS12381 (Fr, fromP, toP) where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Data.Curve.Weierstrass.BLS12381 (Fr)
import Data.Field.Galois (PrimeField (..), fromP, toP)
import qualified Data.Scientific as S
import Juvix.Library hiding (pred)
import Text.Read (Lexeme (..), Read (..), lexP, parens, pfail, step)
import Text.Read.Lex (numberToInteger)

------------------------------------------------------------------------------

instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left d -> panic $ "Can't parse floating:" <> show (d :: Double)
    Right f -> pure $ toP f
  parseJSON j = panic $ "Can't parse non-number:" <> show j

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (fromP f) 0

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
    case numberToInteger n of
      Just i -> pure $ toP i
      Nothing -> pfail

deriving instance Bits Fr
