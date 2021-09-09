{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Core.Erased.Ann.Pretty
  ( PPAnn (..),
    PrimPretty1,
  )
where

import Juvix.Core.Erased.Ann.Types
import Juvix.Core.HR.Pretty (PPAnn, PPAnn' (..), PrimPretty1)
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Library hiding (Type)
import qualified Juvix.Library.PrettyPrint as PP

type instance PP.Ann (Term _ _) = PPAnn

instance PrimPretty1 primVal => PP.PrettySyntax (Term primTy primVal) where
  pretty' = \case
    Var x -> HR.pname x
    Prim p -> fmap HR.toPPAnn <$> PP.pretty' p
    LamM {arguments, body} -> HR.ppLams (arguments, body)
    p@(PairM _ _) -> HR.ppPairs $ getPairs p
    p@(CatProductIntroM _ _) -> HR.ppPairs $ getPairs p
    p@(CatProductElimLeftM _) -> panic "pretty-printing productelimleft not yet implemented"
    p@(CatProductElimRightM _) -> panic "pretty-printing productelimright not yet implemented"
    p@(CatCoproductIntroLeftM _) -> panic "pretty-printing coproductintroleft not yet implemented"
    p@(CatCoproductIntroRightM _) -> panic "pretty-printing coproductintroleft not yet implemented"
    p@(CatCoproductElimM _ _ _) -> HR.ppPairs $ getPairs p
    UnitM -> pure HR.box
    AppM s ts -> HR.ppApps s ts

getPairs :: Term primTy primVal -> [Term primTy primVal]
getPairs (PairM s t) = term s : getPairs (term t)
getPairs (CatProductIntroM s t) = term s : getPairs (term t)
getPairs (CatCoproductElimM cp s t) = term cp : getPairs (term s) ++ getPairs (term t)
getPairs t = [t]

type instance PP.Ann (Type _) = PPAnn

instance PrimPretty1 primTy => PP.PrettySyntax (Type primTy) where
  pretty' = \case
    SymT x -> HR.pname x
    Star i -> HR.ppStar i
    PrimTy t -> fmap HR.toPPAnn <$> PP.pretty' t
    t@(Pi _ _ _) -> HR.ppBinders $ getBinds t
    t@(Sig _ _ _) -> HR.ppBinders $ getBinds t
    t@CatProduct {} -> HR.ppBinders $ getBinds t
    t@CatCoproduct {} -> HR.ppBinders $ getBinds t
    UnitTy -> pure $ PP.annotate' ATyCon "Unit"

getBinds :: Type primTy -> HR.WithBinders (Type primTy)
getBinds = go []
  where
    go acc (Pi π s t) = go (HR.Binder HR.PI π "_" s : acc) t
    go acc (Sig π s t) = go (HR.Binder HR.SIG π "_" s : acc) t
    go acc t = (reverse acc, t)

type instance PP.Ann (AnnTerm _ _) = PPAnn

-- FIXME: do we want to keep the type annotations?
instance PrimPretty1 primVal => PP.PrettySyntax (AnnTerm primTy primVal) where
  pretty' = PP.pretty' . term
