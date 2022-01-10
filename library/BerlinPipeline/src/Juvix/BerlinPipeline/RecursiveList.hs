-- |
--
-- A Recursive list represents the concept of a list with a chance to nest.
-- This allows us to get a tree like structure, where a pipeline step can contain more steps.
module Juvix.BerlinPipeline.RecursiveList where

import Juvix.Library

data T a
  = Rec [T a]
  | Anu a
  deriving (Show, Eq)

instance Semigroup (T a) where
  Rec l <> Rec r = Rec $ l <> r
  Rec l <> a@(Anu _) = Rec $ l ++ [a]
  a@(Anu _) <> Rec r = Rec $ a : r
  l@(Anu _) <> r@(Anu _) = Rec $ l : [r]

removeFirstNested :: T a -> T a
removeFirstNested (Anu a) = Rec []
removeFirstNested (Rec []) = Rec []
removeFirstNested (Rec (x : xs)) =
  case removeFirstNested x of
    Rec [] -> Rec xs
    otherwise -> Rec (otherwise : xs)
