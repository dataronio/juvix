{-# LANGUAGE DeriveGeneric, AllowAmbiguousTypes #-}

-- | Closure.T serves as the data structure in which we will store
-- temporary lexical bindings as our code encounters binders.
module Juvix.Closure.Abstract where

import qualified Data.HashSet as Set
import Data.Hashable (Hashable (..), hash)
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NS
import qualified Juvix.Sexp as Sexp

type MonHashAndName a =  (Monoid a, Hashable a, Show a)

data T a
  = T (Map.T Int a)
  deriving (Show, Eq, Generic)

insert :: (MonHashAndName a, Hashable b) => b -> a -> T a -> T a
insert k info (T m) =
  T $ Map.insert (hash k) info m

insertGeneric :: (MonHashAndName a, Hashable b) => Int -> T a -> T a
insertGeneric name (T m) =
  T $ Map.insert name mempty m

keys :: T a -> Set.HashSet Int
keys (T m) = Map.keysSet m

lookup :: (MonHashAndName a, Hashable b) => b -> T a -> Maybe a
lookup k (T m) = Map.lookup (hash k) m

empty :: MonHashAndName a => T a
empty = T Map.empty

insertHash :: MonHashAndName a => a -> T a -> (Int, T a)
insertHash info t =
  let name = hash info
   in (name, insert name info t)

merge :: MonHashAndName a => T a -> T a -> T a
merge (T m) (T m') = T $ Map.union m m'
