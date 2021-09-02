{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parameterization and application of the LLVM backend primitives.
module Juvix.Backends.LLVM.Parameterization
  ( llvm,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import Juvix.Library.Usage
import qualified LLVM.AST.Type as LLVM

instance Param.CanApply PrimTy where
  -- TODO: Needs to implement apply
  arity = arityTy

instance Param.CanApply (PrimVal ext) where
  type Arg (PrimVal ext) = App.Arg' ext (Param.PrimType PrimTy) RawPrimVal

  -- We need to define @pureArg@ as we overwrite the default @Arg@ definition.
  pureArg = undefined

  arity val = case val of
    App.Cont {} -> App.numLeft val
    App.Return {} -> arityRaw $ App.retTerm val

  -- The following implementation follows the eval/apply method for curried
  -- function application. A description of this can be found in 'How to make a
  -- fast curry: push/enter vs eval/apply' by Simon Marlow and Simon Peyton
  -- Jones.
  -- Given a function, and a non-empty list of arguments, we try to apply the
  -- arguments to the function. The function is of type 'PrimVal''/'Return'',
  -- so either a continuation or a fully evaluated term.
  apply fun args = do
    let (fun', args', funArity') = toTakes fun -- 'args'' are part of continuation fun'
        argLen = lengthN args -- Nr. of free arguments.
        argsAll = foldr NonEmpty.cons args args' -- List of all arguments.
    case argLen `compare` funArity' of
      -- If there are not enough arguments to apply, return a continuation.
      LT ->
        Right $
          App.Cont
            { App.fun = fun',
              App.args = toList argsAll,
              App.numLeft = funArity' - argLen
            }
      -- If there are exactly enough arguments to apply, do so.
      -- In case there aren't any arguments, return a continuation.
      EQ
        | Just returns <- traverse App.argToReturn argsAll ->
          applyProper (takeToReturn fun) returns |> first Param.Extra
        | otherwise ->
          Right $
            App.Cont
              { App.fun = fun',
                App.args = toList argsAll,
                App.numLeft = 0
              }
      -- If there are too many arguments to apply, raise an error.
      GT -> Left $ Param.ExtraArguments fun args
    where
      toTakes App.Cont {App.fun, App.args, App.numLeft} = (fun, args, numLeft)
      toTakes App.Return {App.retType, App.retTerm} = (fun, [], arityRaw retTerm)
        where
          fun =
            App.Take
              { App.usage = Omega,
                App.type' = retType,
                App.term = retTerm
              }

-- TODO
takeToReturn = undefined

-- TODO
applyProper = undefined

-- | Parameters for the LLVM backend.
llvm :: Param.Parameterisation PrimTy RawPrimVal
llvm =
  Param.Parameterisation
    { Param.hasType = hasType,
      Param.builtinTypes = builtinTypes,
      Param.builtinValues = builtinValues,
      Param.stringVal = const Nothing,
      Param.intVal = integerToRawPrimVal,
      Param.floatVal = const Nothing
    }
  where
    -- Typechecking of primitive values.
    hasType :: RawPrimVal -> Param.PrimType PrimTy -> Bool
    hasType t (Param.PrimType ty) = case t of
      Add -> Param.check3Equal ty
      Sub -> Param.check3Equal ty
      LitInt _ -> length ty == 1

    -- The primitive LLVM types available to Juvix users.
    builtinTypes :: Param.Builtins PrimTy
    builtinTypes =
      [("LLVM.int8", PrimTy LLVM.i8)]

    -- The primitive LLVM values available to Juvix users.
    builtinValues :: Param.Builtins RawPrimVal
    builtinValues =
      [ ("LLVM.add", Add),
        ("LLVM.sub", Sub),
        ("LLVM.litint", LitInt 0) -- TODO: what to do with the 0?
      ]

    -- Translate an integer into a RawPrimVal.
    -- TODO: should we take type information into account? As of now, there is
    -- no way to do achieve this due to a lack of information available to the
    -- function.
    integerToRawPrimVal :: Integer -> Maybe RawPrimVal
    integerToRawPrimVal = Just . LitInt

-- | TODO: for now these are just copied over from the Michelson backend.
instance IR.HasWeak PrimTy where weakBy' _ _ t = t

instance IR.HasWeak RawPrimVal where weakBy' _ _ t = t

instance
  Monoid (Core.XVPrimTy ext PrimTy primVal) =>
  IR.HasSubstValue ext PrimTy primVal PrimTy
  where
  substValueWith _ _ _ t = pure $ Core.VPrimTy t mempty

instance
  Monoid (Core.XPrimTy ext PrimTy primVal) =>
  IR.HasPatSubstTerm ext PrimTy primVal PrimTy
  where
  patSubstTerm' _ _ t = pure $ Core.PrimTy t mempty

instance
  Monoid (Core.XPrim ext primTy RawPrimVal) =>
  IR.HasPatSubstTerm ext primTy RawPrimVal RawPrimVal
  where
  patSubstTerm' _ _ t = pure $ Core.Prim t mempty
