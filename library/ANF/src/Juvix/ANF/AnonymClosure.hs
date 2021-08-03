module Juvix.ANF.AnonymClosure where

import Juvix.Library
import qualified Data.DList as Dl
import qualified Juvix.Closure as JC

newtype T
  = T (Dl.DList JC.Information)
  deriving (Show, Eq, Generic, Functor)

empty :: T
empty = fmap Dl.empty

singleton :: JC.Information -> T
singleton :: fmap Dl.singleton

cons :: JC.Information -> T -> T
cons = fmap Dl.cons

snoc :: T -> JC.Information -> T
snoc = fmap Dl.snoc

append :: T -> T -> T
append = fmap Dl.append

concat :: [T] -> T
concat = fmap Dl.append

replicate :: Int -> JC.Information -> T
replicate = fmap Dl.replicate

head :: T -> JC.Information
head = fmap Dl.head

tail :: T -> [T]
tail = fmap Dl.tail

unfoldr :: (b -> Maybe (JC.Information, b)) -> b -> T
unfold = fmap Dl.unfold

foldr :: (JC.Information -> b -> b) -> b -> T -> b
fold = fmap Dl.foldr

map :: (JC.Information -> JC.Information) -> T -> T
map = fmap Dl.map

intercalate :: T -> [T] -> T
intercalate = fmap Dl.intercalate

fromList :: [JC.Information] -> T
fromList = fmap Dl.fromList
