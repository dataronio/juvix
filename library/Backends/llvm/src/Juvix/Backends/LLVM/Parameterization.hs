{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Parameterization and application of the LLVM backend primitives.
module Juvix.Backends.LLVM.Parameterization
  ( llvm,
  )
where

import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
  ( Applicative (pure),
    Bool,
    Eq ((==)),
    Foldable (length),
    Integer,
    Maybe (..),
    Monoid (mempty),
    Text,
    const,
    ($),
    (.),
  )
import qualified LLVM.AST.Type as LLVM

instance Param.CanPrimApply Param.Star PrimTy where
  primArity = arityTy

  -- TODO: Needs to implement apply
  primApply = _

instance Param.CanPrimApply PrimTy RawPrimVal where
  primArity = arityRaw

  -- TODO: Needs to implement apply
  primApply = _

-- | Parameters for the LLVM backend.
llvm :: Param.Parameterisation PrimTy RawPrimVal
llvm =
  Param.Parameterisation
    { Param.hasType = hasType,
      Param.builtinTypes = builtinTypes,
      Param.builtinValues = builtinValues,
      Param.stringVal = stringToRawPrimVal,
      Param.intVal = integerToRawPrimVal,
      Param.floatVal = const Nothing
    }
  where
    -- Typechecking of primitive values.
    hasType :: RawPrimVal -> Param.PrimType PrimTy -> Bool
    hasType t (Param.PrimType ty) = case t of
      Add -> Param.check3Equal ty
      Sub -> Param.check3Equal ty
      Mul -> Param.check3Equal ty
      LitInt _ -> length ty == 1
      LitString _ -> length ty == 1

    -- The primitive LLVM types available to Juvix users.
    builtinTypes :: Param.Builtins PrimTy
    builtinTypes =
      [ ("LLVM.int8", PrimTy LLVM.i8),
        ("LLVM.int16", PrimTy LLVM.i16),
        ("LLVM.string", PrimTy $ Types.pointerOf LLVM.i8)
      ]

    -- The primitive LLVM values available to Juvix users.
    builtinValues :: Param.Builtins RawPrimVal
    builtinValues =
      [ ("LLVM.add", Add),
        ("LLVM.sub", Sub),
        ("LLVM.mul", Mul),
        ("LLVM.litint", LitInt 0) -- TODO: what to do with the 0?
      ]

    -- Translate an integer into a RawPrimVal.
    -- TODO: should we take type information into account? As of now, there is
    -- no way to do achieve this due to a lack of information available to the
    -- function.
    integerToRawPrimVal :: Integer -> Maybe RawPrimVal
    integerToRawPrimVal = Just . LitInt

    stringToRawPrimVal :: Text -> Maybe RawPrimVal
    stringToRawPrimVal = Just . LitString

-- | TODO: for now these are just copied over from the Michelson backend.
instance IR.HasWeak PrimTy where weakBy' _ _ t = t

instance IR.HasWeak RawPrimVal where weakBy' _ _ t = t

instance
  Monoid (Core.XVPrimTy ext PrimTy primVal) =>
  IR.HasSubstValueType ext PrimTy primVal PrimTy
  where
  substValueTypeWith _ _ _ t = pure $ Core.VPrimTy t mempty

instance
  Monoid (Core.XPrimTy ext PrimTy primVal) =>
  IR.HasPatSubstType ext PrimTy primVal PrimTy
  where
  patSubstType' _ _ t = pure $ Core.PrimTy t mempty

instance
  Monoid (Core.XPrim ext primTy RawPrimVal) =>
  IR.HasPatSubstTerm ext primTy RawPrimVal RawPrimVal
  where
  patSubstTerm' _ _ t = pure $ Core.Prim t mempty
