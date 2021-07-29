-- | Closure.T serves as the data structure in which we will store
-- temporary lexical bindings as our code encounters binders.
module Juvix.Closure
  ( Information (..),
    T (..),
    insert,
    insertGeneric,
    keys,
    lookup,
    empty,

    -- * Extended API
    openList,
    registerOpen,
  )
where

import qualified Data.HashSet as Set
import qualified Juvix.Context as Context
import Juvix.Library hiding (Meta, empty)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

-- Currently we don't really use the signature however in the future
-- the mSig will be used to detect the types of modules we will have
-- open and any other information we wish to track here!?
data Information = Info
  { -- | @mSig@ represents the type of the term in the closure
    mSig :: Maybe Sexp.T,
    -- | @info@ represents all the information we have on the term
    info :: [Context.Information],
    -- | @mOpen@ represents a place where the term may have come
    -- from
    mOpen :: Maybe NameSymbol.T
  }
  deriving (Show, Eq)

-- | @Meta@ represents extra information that we may need during some
-- passes. This can be viewed as an extension of the Closure API
newtype Meta = Meta
  { -- Tracking open modules, We don't use it for it's content, but
    -- rather to note metadata that isn't noted by the closure easily
    opens :: [NameSymbol.T]
  }
  deriving (Show, Eq)

data T
  = T (Map.T Symbol Information) Meta
  deriving (Show, Eq)

insert :: Symbol -> Information -> T -> T
insert k info (T m meta) =
  T (Map.insert k info m) meta

insertGeneric :: Symbol -> T -> T
insertGeneric name (T m meta) =
  T (Map.insert name (Info Nothing [] Nothing) m) meta

keys :: T -> Set.HashSet Symbol
keys (T m _meta) = Map.keysSet m

lookup :: Symbol -> T -> Maybe Information
lookup k (T m _meta) = Map.lookup k m

empty :: T
empty = T Map.empty (Meta [])

-- Extended API: Not included with the the normal clsoure API
openList :: T -> [NameSymbol.T]
openList (T _m meta) = opens meta

registerOpen :: T -> NameSymbol.T -> T
registerOpen (T m meta) name = T m (Meta (name : opens meta))
