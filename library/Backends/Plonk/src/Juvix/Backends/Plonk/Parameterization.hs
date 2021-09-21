{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Plonk.Parameterization
  ( hasType,
    builtinTypes,
    builtinValues,
    param,
    integerToPrimVal,
    ApplyError (..),
    arityRaw,
    toArg,
    toTakes,
    fromReturn,
  )
where

import Data.Field.Galois (GaloisField (..))
import qualified Data.List.NonEmpty as NonEmpty
import Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Erased.Ann.Conversion as ErasedAnn
import qualified Juvix.Core.Erased.Ann.Prim as Prim
import qualified Juvix.Core.Erased.Ann.Types as ErasedAnn
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.Parameterisation as Param
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, show, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Prelude (Show (..))

isBool :: PrimTy f -> Bool
isBool PBool = True
isBool _ = False

hasType :: PrimVal f -> Param.PrimType (PrimTy f) -> Bool
hasType tm (Param.PrimType ty) = hasType' tm ty

hasType' :: PrimVal f -> NonEmpty (PrimTy f) -> Bool
hasType' (PConst _v) ty
  | length ty == 1 = True
  | otherwise = False
-- BinOps
hasType' PAdd ty = Param.check3Equal ty
hasType' PSub ty = Param.check3Equal ty
hasType' PMul ty = Param.check3Equal ty
hasType' PDiv ty = Param.check3Equal ty
hasType' PExp ty = Param.check3Equal ty
hasType' PMod ty = Param.check3Equal ty
hasType' PAnd ty = Param.check3Equal ty
hasType' POr ty = Param.check3Equal ty
hasType' PXor ty = Param.check3Equal ty
-- UnOps
hasType' PIsZero ty = Param.check2Equal ty
hasType' PNot ty = Param.check2Equal ty
hasType' PShL ty = Param.check2Equal ty
hasType' PShR ty = Param.check2Equal ty
hasType' PRotL ty = Param.check2Equal ty
hasType' PRotR ty = Param.check2Equal ty
hasType' PAssertEq ty = Param.check2Equal ty
hasType' PAssertIt ty = Param.check2Equal ty
-- CompOps
hasType' PGt ty = Param.checkFirst2AndLast ty isBool
hasType' PGte ty = Param.checkFirst2AndLast ty isBool
hasType' PLt ty = Param.checkFirst2AndLast ty isBool
hasType' PLte ty = Param.checkFirst2AndLast ty isBool
hasType' PEq ty = Param.checkFirst2AndLast ty isBool

builtinTypes :: Param.Builtins (PrimTy f) -- TODO: Revisit this
builtinTypes =
  Map.fromList
    [ (NameSymbol.fromSymbol "Circuit.field", PField),
      (NameSymbol.fromSymbol "Circuit.int", PInt),
      (NameSymbol.fromSymbol "Circuit.bool", PBool)
    ]

builtinValues :: Param.Builtins (PrimVal f)
builtinValues =
  Map.fromList $
    first NameSymbol.fromSymbol
      <$> [ ("Circuit.add", PAdd),
            ("Circuit.sub", PSub),
            ("Circuit.mul", PMul),
            ("Circuit.div", PDiv),
            ("Circuit.exp", PExp),
            ("Circuit.and", PAnd),
            ("Circuit.or", POr),
            ("Circuit.xor", PXor),
            ("Circuit.eq", PEq)
          ] -- TODO: Do the rest

param :: (GaloisField f) => Param.Parameterisation (PrimTy f) (PrimVal f)
param =
  Param.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      stringVal = const Nothing,
      intVal = integerToPrimVal,
      floatVal = const Nothing
    }

integerToPrimVal :: forall f. GaloisField f => Integer -> Maybe (PrimVal f)
integerToPrimVal x = Just . PConst $ fromInteger x

-- copied from michelson; TODO abstract this
instance Core.CanPrimApply Core.Star (PrimTy f) where
  primArity (PApplication hd rest) =
    Core.primArity hd - fromIntegral (length rest)
  primArity x = 0 -- TODO: Refine if/when extending PrimTy

  primApply (App.Take {type', term}) args = pure $ foldl ap1 (type', term) args
    where
      ap1 (Core.PrimType tys0, term) (App.Take {term = arg})
        | (_ :| ty : tys) <- tys0 =
          (Core.PrimType (ty :| tys), ap1' term arg)
        | otherwise =
          -- (ill-typed application)
          panic "primApply @_ @Plonk.PrimTy: too many arguments"
      ap1' (PApplication fun args) arg = PApplication fun (args <> pure arg)
      ap1' fun arg = PApplication fun (pure arg)

data ApplyError f
  = CompilationError (CompilationError f)
  | ReturnTypeNotPrimitive (ErasedAnn.Type (PrimTy f))

instance Show f => Show (ApplyError f) where
  show (CompilationError perr) = show perr
  show (ReturnTypeNotPrimitive ty) =
    "not a primitive type:\n\t" <> show ty

arityRaw :: PrimVal f -> Natural
arityRaw = \case
  PConst _ -> 0
  PIsZero -> 1
  PNot -> 1
  PShL -> 1
  PShR -> 1
  PRotL -> 1
  PRotR -> 1
  PAssertEq -> 1
  PAssertIt -> 1
  -- BinOps
  PAdd -> 2
  PSub -> 2
  PMul -> 2
  PDiv -> 2
  PExp -> 2
  PMod -> 2
  PAnd -> 2
  POr -> 2
  PXor -> 2
  -- CompOps
  PGt -> 2
  PGte -> 2
  PLt -> 2
  PLte -> 2
  PEq -> 2

toArg :: App.Return' ext1 ty term -> Maybe (App.Arg' ext1 ty term)
toArg = Just . App.TermArg

toTakes :: PrimVal' ext f -> (Take f, [Arg' ext f], Natural)
toTakes App.Cont {fun, args, numLeft} = (fun, args, numLeft)
toTakes App.Return {retType, retTerm} = (fun, [], arityRaw retTerm)
  where
    fun = App.Take {usage = Usage.SAny, type' = retType, term = retTerm}

fromReturn :: Return' ext f -> PrimVal' ext f
fromReturn = identity

instance (Integral f, GaloisField f) => Core.CanPrimApply (PrimTy f) (PrimVal f) where
  type PrimApplyError (PrimVal f) = ApplyError f
  primArity = arityRaw
  primApply fun@(App.Take {type', term}) args = do
    retType <- toPrimType $ ErasedAnn.type' newTerm
    pure $
      (retType, PConst $ evalTerm term $ NonEmpty.toList $ App.term <$> args)
    where
      annTerm = ErasedAnn.takeToTerm fun
      annTerms = ErasedAnn.takeToTerm <$> toList args
      newTerm = applyPrimOnArgs annTerm annTerms

evalBinOp :: (Fractional f, Integral f) => PrimVal f -> f -> f -> f
evalBinOp op x y = case op of
  PAdd -> x + y
  PSub -> x - y
  PMul -> x * y
  PDiv -> x / y
  PExp -> x ^ y
  PMod -> x `mod` y
  PAnd -> if x == 0 || y == 0 then 0 else 1
  POr -> if x == 0 then y else x
  PXor -> if (x == 0 && y /= 0) || (x /= 0 && y == 0) then 1 else 0
  PGt -> if x > y then 1 else 0
  PGte -> if x >= y then 1 else 0
  PLt -> if x < y then 1 else 0
  PLte -> if x <= y then 1 else 0
  PEq -> if x == y then 1 else 0

evalTerm :: (Fractional f, Integral f) => PrimVal f -> [PrimVal f] -> f
evalTerm val vals = case val of
  PConst f -> f
  _ | (PConst x : PConst y : _) <- vals -> evalBinOp val x y
  _ | (PConst x : _) <- vals -> panic "TODO: Implement evalUnOp" --evalUnOp val x y
  _ -> panic "Not implemented"

-- | Given a type, translate it to a type in the Plonk backend.
toPrimType :: ErasedAnn.Type (PrimTy f) -> Either (ApplyError f) (Core.PrimType (PrimTy f))
toPrimType ty = maybe err (Right . Core.PrimType) $ go ty
  where
    err = Left $ ReturnTypeNotPrimitive ty
    go ty = goPi ty <|> (pure <$> goPrim ty)
    goPi (ErasedAnn.Pi _ s t) = NonEmpty.cons <$> goPrim s <*> go t
    goPi _ = Nothing
    goPrim (ErasedAnn.PrimTy p) = Just p
    goPrim _ = Nothing

applyPrimOnArgs :: Types.AnnTerm f -> [Types.AnnTerm f] -> Types.AnnTerm f
applyPrimOnArgs prim arguments =
  let newTerm = ErasedAnn.AppM prim arguments
      retType = ErasedAnn.piToReturnType (ErasedAnn.type' prim)
   in ErasedAnn.Ann one retType newTerm

instance Eval.HasWeak (PrimTy f) where weakBy' _ _ t = t

instance Eval.HasWeak (PrimVal f) where weakBy' _ _ t = t

instance
  Monoid (Core.XVPrimTy ext (PrimTy f) primVal) =>
  Eval.HasSubstValueType ext (PrimTy f) primVal (PrimTy f)
  where
  substValueTypeWith _ _ _ t = pure $ Core.VPrimTy t mempty

instance
  Monoid (Core.XPrimTy ext (PrimTy f) primVal) =>
  Eval.HasPatSubstType ext (PrimTy f) primVal (PrimTy f)
  where
  patSubstType' _ _ t = pure $ Core.PrimTy t mempty

instance
  Monoid (Core.XPrim ext primTy (PrimVal f)) =>
  Eval.HasPatSubstTerm ext primTy (PrimVal f) (PrimVal f)
  where
  patSubstTerm' _ _ t = pure $ Core.Prim t mempty
