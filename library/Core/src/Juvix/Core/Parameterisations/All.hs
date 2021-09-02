{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Core.Parameterisations.All
  ( Ty (..),
    Val (..),
    t,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as E
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Parameterisations.Naturals as Naturals
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Library hiding ((<|>))
import Prelude (error)

-- all primitive types
data Ty
  = NatTy Naturals.Ty
  | UnitTy Unit.Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = NatVal Naturals.Val
  | UnitVal Unit.Val
  deriving (Show, Eq)

unNatTy :: Ty -> Maybe Naturals.Ty
unNatTy (NatTy t) = pure t
unNatTy _ = empty

unUnitTy :: Ty -> Maybe Unit.Ty
unUnitTy (UnitTy t) = pure t
unUnitTy _ = empty

hasType :: Val -> P.PrimType Ty -> Bool
hasType (NatVal x) (traverse unNatTy -> Just tys) = Naturals.hasType x tys
hasType (UnitVal x) (traverse unUnitTy -> Just tys) = Unit.hasType x tys
hasType _ _ = False

instance P.CanPrimApply () Ty where
  primArity _ = 0
  primApply _ _ = error "ill typed"

instance P.CanPrimApply Ty Val where
  primArity (NatVal x)  = P.primArity x
  primArity (UnitVal x) = P.primArity x

  primApply (unNatValT -> Just f) (traverse unNatValT -> Just xs) =
    second (bimap (fmap NatTy) NatVal) $ P.primApply f xs
  primApply _ _ = error "ill typed"

instance E.HasWeak Ty where weakBy' _ _ ty = ty

instance Monoid (Core.XVPrimTy ext Ty val) => E.HasSubstValue ext Ty val Ty where
  substValueWith _ _ _ ty = pure $ Core.VPrimTy ty mempty

instance Monoid (Core.XPrimTy ext Ty val) => E.HasPatSubstTerm ext Ty val Ty where
  patSubstTerm' _ _ ty = pure $ Core.PrimTy ty mempty

instance E.HasWeak Val where weakBy' _ _ val = val

instance Monoid (Core.XVPrim ext ty Val) => E.HasSubstValue ext ty Val Val where
  substValueWith _ _ _ val = pure $ Core.VPrim val mempty

instance Monoid (Core.XPrim ext Ty Val) => E.HasPatSubstTerm ext Ty Val Val where
  patSubstTerm' _ _ val = pure $ Core.Prim val mempty

unNatValT ::
  App.Take (P.PrimType Ty) Val ->
  Maybe (App.Take (P.PrimType Naturals.Ty) Naturals.Val)
unNatValT (App.Take {usage, type' = type'', term = term'})
  | Just type' <- traverse unNatTy type'',
    Just term <- unNatVal term' =
    Just $ App.Take {usage, type', term}
unNatValT (App.Take {}) = Nothing

unNatVal :: Val -> Maybe Naturals.Val
unNatVal (NatVal n) = Just n
unNatVal _ = Nothing

builtinTypes :: P.Builtins Ty
builtinTypes =
  fmap NatTy Naturals.builtinTypes
    <> fmap UnitTy Unit.builtinTypes

builtinValues :: P.Builtins Val
builtinValues =
  fmap NatVal Naturals.builtinValues
    <> fmap UnitVal Unit.builtinValues

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = fmap NatVal . Naturals.natVal,
      floatVal = const Nothing
    }
