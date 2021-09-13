{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Core.Erased.Ann.Pretty
  ( PrimPretty1,
  )
where

------------------------------------------------------------------------------

import Juvix.Core.Erased.Ann.Types
  ( AnnTerm (term),
    Term
      ( AppM,
        CatCoproductElimM,
        CatCoproductIntroLeftM,
        CatCoproductIntroRightM,
        CatProductElimLeftM,
        CatProductElimRightM,
        CatProductIntroM,
        LamM,
        PairM,
        Prim,
        UnitM,
        Var,
        arguments,
        body
      ),
    Type (..),
  )
import Juvix.Core.HR.Pretty (PPAnn, PPAnn' (..), PrimPretty1)
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Library
  ( Applicative (pure),
    Functor (fmap),
    reverse,
    ($),
    (.),
    (<$>),
  )
import qualified Juvix.Library.PrettyPrint as PP

------------------------------------------------------------------------------

type instance PP.Ann (Term _ _) = PPAnn

instance PrimPretty1 primVal => PP.PrettySyntax (Term primTy primVal) where
  pretty' = \case
    Var x -> HR.pname x
    Prim p -> fmap HR.toPPAnn <$> PP.pretty' p
    LamM {arguments, body} -> HR.ppLams (arguments, body)
    p@(PairM _ _) -> HR.ppPairs $ getPairs p
    CatProductIntroM t1 t2 -> HR.ppCatProductIntro t1 t2
    CatProductElimLeftM _ t -> HR.ppCatProductElimLeft t
    CatProductElimRightM _ t -> HR.ppCatProductElimRight t
    CatCoproductIntroLeftM t -> HR.ppCatCoproductIntroLeft t
    CatCoproductIntroRightM t -> HR.ppCatCoproductIntroRight t
    CatCoproductElimM _ _ match c1 c2 -> HR.ppCatCoproductElim match c1 c2
    UnitM -> pure HR.unitVal
    AppM s ts -> HR.ppApps s ts

getPairs :: Term primTy primVal -> [Term primTy primVal]
getPairs (PairM s t) = term s : getPairs (term t)
getPairs t = [t]

type instance PP.Ann (Type _) = PPAnn

instance PrimPretty1 primTy => PP.PrettySyntax (Type primTy) where
  pretty' = \case
    SymT x -> HR.pname x
    Star i -> HR.ppStar i
    PrimTy t -> fmap HR.toPPAnn <$> PP.pretty' t
    t@Pi {} -> HR.ppBinders $ getBinds t
    t@Sig {} -> HR.ppBinders $ getBinds t
    CatProduct a b -> HR.ppCatProduct a b
    CatCoproduct a b -> HR.ppCatCoproduct a b
    UnitTy -> pure $ PP.annotate' ATyCon "Unit"

getBinds :: Type primTy -> HR.WithBinders (Type primTy)
getBinds = go []
  where
    go ::
      [HR.Binder (Type primTy)] ->
      Type primTy ->
      HR.WithBinders (Type primTy)
    go acc (Pi π s t) = go (HR.Binder HR.PI π "_" s : acc) t
    go acc (Sig π s t) = go (HR.Binder HR.SIG π "_" s : acc) t
    go acc t = (reverse acc, t)

type instance PP.Ann (AnnTerm _ _) = PPAnn

-- FIXME: do we want to keep the type annotations?
instance PrimPretty1 primVal => PP.PrettySyntax (AnnTerm primTy primVal) where
  pretty' = PP.pretty' . term
