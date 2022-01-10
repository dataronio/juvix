module Juvix.BerlinPipeline.CircularList where

import qualified Juvix.BerlinPipeline.RecursiveList as RecList
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T a = T (RecList.T (CircSchema a))
  deriving (Show, Eq)

instance Semigroup (T a) where
  T l <> T r = T (l <> r)

data CircSchema a
  = CircSchema [a]
  | NonCircSchema a
  deriving (Show, Eq)

firstNested :: T a -> Maybe (CircSchema a)
firstNested (T (RecList.Anu a)) = Just a
firstNested (T (RecList.Rec [])) = Nothing
firstNested (T (RecList.Rec (x : _xs))) = firstNested (T x)

removeFirstNested :: T a -> T a
removeFirstNested (T l) = T $ RecList.removeFirstNested l

init :: NameSymbol.T -> T a
init sym = T (RecList.Rec [])

initAnu :: a -> T a
initAnu v = T (RecList.Anu (NonCircSchema v))

empty :: T a
empty = T (RecList.Rec [])
