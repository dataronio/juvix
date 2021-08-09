module Juvix.ANF.Stack where

import Juvix.Library
import qualified Data.DList as Dl
import qualified Juvix.Closure as JC

newtype T a
  = T (Dl.DList a)
  deriving (Show, Eq, Generic, Functor)

empty :: T a
empty = fmap Dl.empty

singleton :: a -> T a
singleton :: fmap Dl.singleton

cons :: a -> T a -> T a
cons = fmap Dl.cons

snoc :: T a -> a -> T a
snoc = fmap Dl.snoc

append :: T a -> T a -> T a
append = fmap Dl.append

concat :: [T a] -> T a
concat = fmap Dl.append

replicate :: Int -> a -> T a
replicate = fmap Dl.replicate

head :: T a -> a
head = fmap Dl.head

tail :: T a -> [T a]
tail = fmap Dl.tail

unfoldr :: (b -> Maybe (a, b)) -> b -> T a
unfold = fmap Dl.unfold

foldr :: (a -> b -> b) -> b -> T a -> b
fold = fmap Dl.foldr

map :: (a -> b) -> T a -> T b
map = fmap Dl.map

intercalate :: T a -> [T a] -> T a
intercalate = fmap Dl.intercalate

fromList :: [a] -> T a
fromList = fmap Dl.fromList

infixr 5 :>
pattern (:>) :: T a -> T a -> T a
pattern (a :> as) = T (Dl.Cons a as)
pattern (a :> _) = T (Dl.Con a Dl.Nil)
