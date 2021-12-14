{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Juvix.Sexp.Serialize where

import qualified Data.Char8 as Char8
import GHC.Generics as Generics
-- import qualified GHC.Types as Types
import Juvix.Library hiding (foldr)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Sexp.Types as Sexp hiding (double)

class Serialize a where
  serialize :: a -> T
  default serialize :: (Generic a, GSerialize (Rep a)) => a -> T
  serialize t = gput (from t)

  deserialize :: T -> Maybe a
  default deserialize :: (Generic a, GSerialize (Rep a)) => T -> Maybe a
  deserialize t = to <$> gget t

class GSerialize f where
  gput :: f a -> T
  gget :: T -> Maybe (f a)

----------------------------------------
-- U1
----------------------------------------

instance GSerialize U1 where
  gput U1 = Nil
  gget _xs = Just U1

----------------------------------------
-- M1
----------------------------------------

instance (GSerialize a) => GSerialize (D1 i a) where
  gput (M1 x) =
    gput x
  gget xs = M1 <$> gget xs

-- Make an alternative version that cares about the selector
-- constructor
instance (Selector i, GSerialize a) => GSerialize (S1 i a) where
  gput y@(M1 x) =
    gput x
  gget xs =
    -- see if car is correct
    M1 <$> gget (car xs)

-- can we make consturctors with no arguments not be a list!?  for
-- example if we have @| Test@ We want it not to be (:test), but :test
instance (Constructor i, GSerialize a) => GSerialize (C1 i a) where
  gput y@(M1 x) =
    let name = mlNameToReservedLispName (conName y)
     in case gput x of
          Sexp.Nil -> Sexp.Atom (A name Nothing)
          otherwis -> Cons (Sexp.Atom (A name Nothing)) otherwis
  gget xs =
    case xs of
      Cons (Atom (A nameOf _)) _ -> logic nameOf
      Atom (A nameOfSingleCon _) -> logic nameOfSingleCon
      __________________________ -> Nothing
    where
      -- we need to cdr past the argument
      maybeX = gget (cdr xs)
      logic name1 =
        case maybeX of
          Just t ->
            let m1 = M1 t
                name = conName m1
             in if
                    | name1 == mlNameToReservedLispName name -> Just m1
                    | otherwise -> Nothing
          Nothing -> Nothing

----------------------------------------
-- Sum
----------------------------------------

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = gput x
  gput (R1 x) = gput x

  -- is this correct?
  gget xs = (L1 <$> gget xs) <|> (R1 <$> gget xs)

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput (a :*: b) = append (gput a) (gput b)

  -- is this correct also!?
  gget xs = (:*:) <$> gget xs <*> gget (cdr xs)

----------------------------------------
-- K1
----------------------------------------

instance Serialize a => GSerialize (K1 i a) where
  gput (K1 x) = Sexp.Cons (serialize x) Nil
  gget xs = K1 <$> deserialize xs

mlNameToReservedLispName :: [Char] -> NameSymbol.T
mlNameToReservedLispName [] = NameSymbol.fromString ""
mlNameToReservedLispName (x : xs) =
  let upperToDash x
        | Char8.isUpper x = ['-', Char8.toLower x]
        | otherwise = [x]
      properName =
        xs >>= upperToDash
   in NameSymbol.fromString (':' : Char8.toLower x : properName)

instance Serialize Integer where
  serialize i = Atom (N i Nothing)
  deserialize (Atom (N i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize () where
  serialize () = Nil
  deserialize Nil = Just ()
  deserialize _ = Nothing

foldr :: (Base a -> p -> p) -> p -> Base a -> p
foldr f acc ts =
  case ts of
    Cons a as -> f a (foldr f acc as)
    Atom ____ -> f ts acc
    Nil -> acc

append :: Base a -> Base a -> Base a
append xs ys = foldr Cons ys xs

-- | @car@ grabs the head of the list
car :: Base a -> Base a
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

-- | @cdr@ grabs the tail of the list
cdr :: Base a -> Base a
cdr (Cons _ xs) = xs
cdr Nil = Nil
cdr (Atom a) = Atom a
