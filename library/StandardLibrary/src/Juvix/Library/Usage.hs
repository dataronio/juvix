{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Juvix.Library.Usage
  ( Usage (..),
    T,
    toUsage,
    allows,
    pred,
    minus,
  )
where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Juvix.Library hiding (pred, show)
import qualified Juvix.Library.PrettyPrint as PP

------------------------------------------------------------------------------

-- | Usage is the type ℕ + ω.
data Usage = SNat Natural | Omega
  deriving (Eq, Show, Read, Generic, Data, NFData)

type T = Usage

------------------------------------------------------------------------------
-- Usage forms an ordered semiring (ℕ+ω, (+), 0, (*), 1).
------------------------------------------------------------------------------

instance Semigroup Usage where
  SNat x <> SNat y = SNat (x + y)
  Omega <> _ = Omega
  _ <> Omega = Omega

instance Monoid Usage where
  mempty = SNat 0

instance Semiring Usage where
  one = SNat 1

  -- Operation
  SNat x <.> SNat y = SNat (x * y)
  Omega <.> _ = Omega
  _ <.> Omega = Omega

instance Ord Usage where
  compare (SNat a) (SNat b) = compare a b
  compare (SNat _) Omega = LT
  compare Omega (SNat _) = GT
  compare Omega Omega = EQ

------------------------------------------------------------------------------

type instance PP.Ann Usage = ()

instance PP.PrettySyntax Usage where
  pretty' (SNat π) = pure . PP.show $ π
  pretty' Omega = pure "ω"

instance A.ToJSON Usage where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Usage where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

-- | toUsage is a helper function that converts an integer to NatAndW
toUsage :: Integer -> Usage
toUsage = SNat . fromInteger

pred :: Usage -> Usage
pred (SNat x) = SNat (x - 1)
pred Omega = Omega

minus :: Usage -> Usage -> Maybe Usage
minus (SNat i) (SNat j)
  | i >= j = Just . SNat $ i - j
minus Omega _ = Just Omega
minus _ _ = Nothing

infixl 6 `minus`

-- | allows is the function that checks usage compatibility
allows :: Usage -> Usage -> Bool
allows (SNat x) (SNat y) = x == y
allows (SNat _) Omega = False
allows Omega (SNat _) = True
allows Omega Omega = True

infix 4 `allows`
