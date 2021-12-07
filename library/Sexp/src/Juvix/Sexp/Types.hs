{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Sexp.Types where

import Control.Lens hiding (List, from, (:>), (|>))
import qualified Data.Aeson as A
import Data.Hashable ()
import Juvix.Library hiding (show, toList)
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Library.NameSymbol as NameSymbol
import Prelude (Show (..), String)

type T = Base ()

-- TODO ∷ make Atom generic, and have it conform to an interface?
-- This way we can erase information later!
data Base a
  = Atom (Atom a)
  | Cons {tCar :: Base a, tCdr :: Base a}
  | Nil
  deriving (Eq, Data, Generic, NFData)

instance A.ToJSON T where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON T where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Atom a
  = A {atomName :: NameSymbol.T, atomLineNum :: Maybe LineNum.T}
  | N {atomNum :: Integer, atomLineNum :: Maybe LineNum.T}
  | D {atomDouble :: Double, atomLineNum :: Maybe LineNum.T}
  | S {atomText :: Text, atomLineNum :: Maybe LineNum.T}
  | P {atomTerm :: a, atomLineNum :: Maybe LineNum.T}
  deriving (Show, Data, Generic, NFData)

instance A.ToJSON a => A.ToJSON (Atom a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON a => A.FromJSON (Atom a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

noLoc :: Atom a -> Atom a
noLoc a = a {atomLineNum = Nothing}

instance Eq a => Eq (Atom a) where
  (==) = (==) `on` from . noLoc

instance Ord a => Ord (Atom a) where
  compare = compare `on` from . noLoc

instance Hashable a => Hashable (Atom a) where
  hash (D {atomDouble}) = hash ('D', atomDouble)
  hash (P {atomTerm}) = hash ('P', atomTerm)
  hash (S {atomText}) = hash ('S', atomText)
  hash (A {atomName}) = hash ('A', atomName)
  hash (N {atomNum}) = hash ('N', atomNum)

instance Hashable T where
  hash (Atom atom) = hash atom
  hash Nil = 1
  hash (Cons {tCar, tCdr}) = hash (hash tCar, hash tCdr)

makeLensesWith camelCaseFields ''Atom

toList' :: Base a -> ([Base a], Maybe (Atom a))
toList' (Cons x xs) = first (x :) $ toList' xs
toList' Nil = ([], Nothing)
toList' (Atom a) = ([], Just a)

toList :: Alternative f => T -> f [T]
toList s = case toList' s of (xs, Nothing) -> pure xs; _ -> empty

infixr 5 :>

pattern (:>) :: Base a -> Base a -> Base a
pattern x :> xs = Cons x xs

{-# COMPLETE (:>), Atom, Nil #-}

pattern List :: [Base a] -> Base a
pattern List xs <-
  (toList' -> (xs, Nothing))
  where
    List xs = foldr Cons Nil xs

pattern IList :: [Base a] -> Atom a -> Base a
pattern IList xs a <-
  (toList' -> (xs, Just a))
  where
    IList xs a = foldr Cons (Atom a) xs

{-# COMPLETE List, IList, Atom #-}

-- TODO ∷ make reader instance

-- TODO ∷ this is poorly written, please simplify

instance Show a => Show (Base a) where
  show (Cons car (Atom a)) =
    "(" <> show car <> " . " <> show (Atom a) <> ")"
  show (Cons car cdr)
    | take 1 (showNoParens cdr) == ")" =
      "(" <> show car <> showNoParens cdr
    | otherwise =
      "(" <> show car <> " " <> showNoParens cdr
  show (Atom (A x _)) =
    show (NameSymbol.toSymbol x)
  show (Atom (N x _)) =
    show x
  show (Atom (D x _)) =
    show x
  show (Atom (S x _)) =
    show x
  show (Atom (P x _)) =
    "#S(" <> show x <> ")"
  show Nil = "()"

showNoParens :: Show a => Base a -> String
showNoParens (Cons car (Atom a)) =
  show car <> " . " <> show (Atom a) <> ")"
showNoParens (Cons car cdr)
  | showNoParens cdr == ")" =
    show car <> showNoParens cdr
  | otherwise =
    show car <> " " <> showNoParens cdr
showNoParens Nil = ")"
showNoParens xs = show xs
