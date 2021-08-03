{-# LANGUAGE DeriveGeneric #-}

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

type HashableAndName a b =  (Hashable a, NS.Name b)

data T a where
  T :: Hashable a b => Map.T b a -> T (Map.T b a)
  deriving (Show, Eq, Generic)

insert :: HashableAndName a b => b -> a -> T -> T
insert k info (T m) =
  T $ Map.insert k info m

insertGeneric :: Name a => a -> T -> T
insertGeneric name (T m) =
  T $ Map.insert name (Info Nothing [] Nothing) m

keys :: Name a => T -> Set.HashSet a
keys (T m) = Map.keysSet m

lookup :: HashableAndName a b => b -> T -> Maybe a
lookup k (T m) = Map.lookup k m

empty :: T
empty = T Map.empty

insertHash :: HashableAndName a b => a -> T -> (b, T)
insertHash info t =
  let name = intern . show $ hash info
   in (name, insertGeneric name info t)

merge :: T -> T -> T
merge = fmap Map.union
