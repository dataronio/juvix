{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisations.Naturals
  ( Ty (..),
    hasType,
    typeOf,
    Val (..),
    builtinTypes,
    builtinValues,
    isNat,
    natVal,
    t,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Evaluator as E
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (natVal, (<|>))
import qualified Juvix.Library.PrettyPrint as PP
import Text.Show

-- k: primitive type: naturals
data Ty
  = Ty
  deriving (Show, Eq)

-- c: primitive constant and f: functions
data Val
  = Val Natural -- c
  | Add -- f addition
  | Sub -- f subtraction
  | Mul -- f multiplication
  | Curried Val Natural
  deriving (Eq)

instance Show Val where
  show (Val x) = "Nat " <> Text.Show.show x
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show (Curried x y) = Juvix.Library.show x <> " " <> Text.Show.show y

typeOf :: Val -> P.PrimType Ty
typeOf (Val _) = P.PrimType $ Ty :| []
typeOf (Curried _ _) = P.PrimType $ Ty :| [Ty]
typeOf Add = P.PrimType $ Ty :| [Ty, Ty]
typeOf Sub = P.PrimType $ Ty :| [Ty, Ty]
typeOf Mul = P.PrimType $ Ty :| [Ty, Ty]

hasType :: Val -> P.PrimType Ty -> Bool
hasType x ty = ty == typeOf x

instance P.CanPrimApply () Ty where
  primArity Ty = 0
  primApply _ _ = panic "ill typed"

instance P.CanPrimApply Ty Val where
  primArity = pred . fromIntegral . length . typeOf
  primApply f xs = app (App.term f) $ map App.term (toList xs)
    where
      app n [] = Right (typeOf n, n)
      app Add (Val x : xs) = app (Curried Add x) xs
      app Sub (Val x : xs) = app (Curried Sub x) xs
      app Mul (Val x : xs) = app (Curried Mul x) xs
      app (Curried Add x) (Val y : ys) = app (Val (x + y)) ys
      app (Curried Sub x) (Val y : ys) = app (Val (x - y)) ys
      app (Curried Mul x) (Val y : ys) = app (Val (x * y)) ys
      app _ (_ : _) = panic "ill typed"

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

isNat :: Integer -> Bool
isNat i = i >= 0

natVal :: Integer -> Maybe Val
natVal i = if i >= 0 then Just (Val (fromIntegral i)) else Nothing

builtinTypes :: P.Builtins Ty
builtinTypes = HM.fromList [("Nat" :| [], Ty)]

builtinValues :: P.Builtins Val
builtinValues =
  HM.fromList
    [ ("add" :| [], Add),
      ("sub" :| [], Sub),
      ("mul" :| [], Mul)
    ]

t :: P.Parameterisation Ty Val
t =
  P.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = natVal,
      floatVal = const Nothing
    }

type instance PP.Ann Ty = ()

instance PP.PrettySyntax Ty where pretty' Ty = pure "Nat"

data PPAnn' = Lit | Fun | Paren deriving (Eq, Ord, Show)

type PPAnn = Last PPAnn'

type Doc = PP.Doc PPAnn

type instance PP.Ann Val = PPAnn

nat :: Natural -> Doc
nat = PP.annotate' Lit . PP.show

pnat :: Applicative f => Natural -> f Doc
pnat = pure . nat

instance PP.PrettySyntax Val where
  pretty' = \case
    Val k -> pnat k
    Add -> pure $ PP.annotate' Fun "add"
    Sub -> pure $ PP.annotate' Fun "sub"
    Mul -> pure $ PP.annotate' Fun "mul"
    Curried f k -> PP.app' Paren (PP.pretty' f) [pnat k]

instance HR.ToPPAnn PPAnn where
  toPPAnn = fmap \case
    Lit -> HR.APrimVal
    Fun -> HR.APrimFun
    Paren -> HR.APunct
