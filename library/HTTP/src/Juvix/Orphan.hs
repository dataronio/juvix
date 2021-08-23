{-# LANGUAGE DeriveAnyClass #-}

module Juvix.Orphan where

import qualified Data.Aeson as A
import Data.Curve.Weierstrass.BLS12381 (Fr)
import Data.Field.Galois (GaloisField, PrimeField (..), toP)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Scientific as S
import Juvix.Library

deriving instance Bits Fr

instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left floating -> panic $ "Can't parse floating :" <> show n
    Right f -> pure . toP $ toInteger f

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (fromP f) 0
