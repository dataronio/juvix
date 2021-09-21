{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisations.Unit
  ( Ty (..),
    hasType,
    Val (..),
    builtinTypes,
    builtinValues,
    t,
  )
where

import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as E
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding ((<|>))

-- k: primitive type: unit
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val
  deriving (Show, Eq)

hasType :: Val -> P.PrimType Ty -> Bool
hasType Val (P.PrimType (Ty :| [])) = True
hasType _ _ = False

instance P.CanPrimApply P.Star Ty where
  primArity _ = 0
  primApply _ _ = panic "ill typed"

instance P.CanPrimApply Ty Val where
  primArity _ = 0
  primApply _ _ = panic "ill typed"

instance E.HasWeak Ty where weakBy' _ _ ty = ty

instance Monoid (Core.XVPrimTy ext Ty val) => E.HasSubstValueType ext Ty val Ty where
  substValueTypeWith _ _ _ ty = pure $ Core.VPrimTy ty mempty

instance Monoid (Core.XPrimTy ext Ty val) => E.HasPatSubstType ext Ty val Ty where
  patSubstType' _ _ ty = pure $ Core.PrimTy ty mempty

instance E.HasWeak Val where weakBy' _ _ val = val

instance Monoid (Core.XVPrim ext ty Val) => E.HasSubstValue ext ty Val Val where
  substValueWith _ _ _ val = pure $ Core.VPrim val mempty

instance Monoid (Core.XPrim ext Ty Val) => E.HasPatSubstTerm ext Ty Val Val where
  patSubstTerm' _ _ val = pure $ Core.Prim val mempty

builtinTypes :: P.Builtins Ty
builtinTypes = [(["Unit"], Ty)]

builtinValues :: P.Builtins Val
builtinValues = [(["tt"], Val)]

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = const Nothing,
      floatVal = const Nothing
    }
