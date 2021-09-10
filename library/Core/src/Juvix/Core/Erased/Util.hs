module Juvix.Core.Erased.Util where

import qualified Data.Set as Set
import Juvix.Core.Erased.Types
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Retrieve the list of variable names that are not bound,
-- i.e. those that are free
free :: Term primVal -> [NameSymbol.T]
free = Set.toList . go Set.empty
  where
    go used = \case
      Var s -> if Set.member s used then Set.empty else Set.singleton s
      Prim _ -> Set.empty
      Lam v b -> go (Set.insert v used) b
      Pair s t -> go used s `Set.union` go used t
      CatProductIntro s t -> go used s `Set.union` go used t
      CatProductElimLeft a s -> go used a `Set.union` go used s
      CatProductElimRight a s -> go used a `Set.union` go used s
      CatCoproductIntroLeft s -> go used s
      CatCoproductIntroRight s -> go used s
      CatCoproductElim a b cp s t -> go used a `Set.union` go used b `Set.union` go used cp `Set.union` go used s `Set.union` go used t
      Unit -> Set.empty
      Let v b t -> go used b `Set.union` go (Set.insert v used) t
      App a b -> go used a `Set.union` go used b
