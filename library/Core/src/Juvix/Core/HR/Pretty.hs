{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Core.HR.Pretty
  ( module Juvix.Core.Pretty,
  -- also instances of PrettySyntax
  )
where

import Juvix.Core.HR.Types
import Juvix.Core.Pretty
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

type instance Ann (Term _ _) = PPAnn

type instance Ann (Elim _ _) = PPAnn

type instance Ann (Pattern _ _) = PPAnn

instance PrimPretty primTy primVal => PrettySyntax (Term primTy primVal) where
  pretty' = \case
    -- Universe types
    Star i -> ppStar i
    -- Primitive types
    PrimTy ty -> annotate' APrimTy . fmap toPPAnn <$> pretty' ty
    Prim val -> annotate' APrimVal . fmap toPPAnn <$> pretty' val
    -- Pi-types
    t@Pi {} -> ppBinders $ getBinds t
    t@(Lam _ _) -> ppLams $ getLams t
    -- Sigma-types
    t@Sig {} -> ppBinders $ getBinds t
    t@(Pair _ _) -> ppPairs $ getPairs t
    -- Product types
    CatProduct a b -> ppCatProduct a b
    CatProductIntro t1 t2 -> ppCatProductIntro t1 t2
    CatProductElimLeft _ t -> ppCatProductElimLeft t
    CatProductElimRight _ t -> ppCatProductElimRight t
    -- Coproduct types
    CatCoproduct s t -> ppCatCoproduct s t
    CatCoproductIntroLeft t -> ppCatCoproductIntroLeft t
    CatCoproductIntroRight t -> ppCatCoproductIntroRight t
    CatCoproductElim _ _ match c1 c2 -> ppCatCoproductElim match c1 c2
    -- Unit type
    UnitTy -> pure unitCon
    Unit -> pure unitVal
    -- Others terms.
    Let π x b t -> ppLet π x b t
    UnitTy -> pure $ annotate' ATyCon "Unit"
    Unit -> pure box
    Elim e -> pretty' e

instance PrimPretty primTy primVal => PrettySyntax (Elim primTy primVal) where
  pretty' = \case
    Var x -> pname x
    App f s -> ppApps f' $ ss <> [s]
      where
        (f', ss) = getApps f
    Ann π s a ->
      parens
        <$> hangA
          indentWidth
          (hsepA [ppUsage π, pure pipe, ppOuter s])
          (hsepA [pure colon, ppOuter a])

instance
  PrimPretty primTy primVal =>
  PrettySyntax (Pattern primTy primVal)
  where
  pretty' = \case
    PCon k ps -> app' APunct (pname k) (map pretty' ps)
    PPair a b -> ppPairs [a, b]
    PUnit -> pure box
    PVar x -> pname x
    PDot s -> hsepA [pure dot, withPrec FunArg $ pretty' s]
    PPrim p -> fmap toPPAnn <$> pretty' p

getLams :: Term primTy primVal -> ([NameSymbol.T], Term primTy primVal)
getLams = go []
  where
    go acc (Lam x t) = go (x : acc) t
    go acc t = (reverse acc, t)

getBinds :: Term primTy primVal -> WithBinders (Term primTy primVal)
getBinds = go []
  where
    go acc (Pi π x s t) = go (Binder PI π x s : acc) t
    go acc (Sig π x s t) = go (Binder SIG π x s : acc) t
    go acc t = (reverse acc, t)

getApps :: Elim primTy primVal -> (Elim primTy primVal, [Term primTy primVal])
getApps = go []
  where
    go acc (App f s) = go (s : acc) f
    go acc e = (e, reverse acc)

-- | Extract a right-nested tuple.
getPairs :: Term primTy primVal -> [Term primTy primVal]
getPairs (Pair s t) = s : getPairs t
getPairs t = [t]
