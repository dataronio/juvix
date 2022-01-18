{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

-- | Module that implements the backend parameters for the Michelson backend.
module Juvix.Backends.Michelson.Parameterisation
  ( michelson,
    module Types,
    module Pretty,
  )
where

import qualified Control.Arrow as Arr
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.Michelson.Compilation as Compilation
import Juvix.Backends.Michelson.Compilation.Pretty as Pretty
import Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Contract as Contract ()
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.InstructionsEff as Run
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpreter
import qualified Juvix.Backends.Michelson.DSL.Untyped as DSLU
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Core.Pretty as PP
import qualified Juvix.Core.Types as Core
import Juvix.Library hiding (many, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Morley.Michelson.Text as M
import qualified Morley.Michelson.Untyped as M
import qualified Morley.Michelson.Untyped.Type as Untyped
import Prelude (Show (..), error)

-- | Can the given type be used as a boolean? Note that in addition to
-- booleans, integers and natural numbers re valid is well.
isBool :: RawPrimTy -> Bool
isBool (PrimTy (M.Ty M.TBool _)) = True
isBool (PrimTy (M.Ty M.TInt _)) = True
isBool (PrimTy (M.Ty M.TNat _)) = True
isBool _ = False

-- | Check if the value has the given type.
hasType :: RawPrimVal -> P.PrimType RawPrimTy -> Bool
hasType tm (P.PrimType ty) = hasType' tm ty

hasType' :: RawPrimVal -> NonEmpty RawPrimTy -> Bool
hasType' AddTimeStamp ty = P.check3Equal ty
hasType' AddI ty = P.check3Equal ty
hasType' AddN ty = P.check3Equal ty
hasType' SubI ty = P.check3Equal ty
hasType' SubN ty = P.check3Equal ty
hasType' SubTimeStamp ty = P.check3Equal ty
hasType' MulI ty = P.check3Equal ty
hasType' MulN ty = P.check3Equal ty
hasType' MulMutez ty = P.check3Equal ty
hasType' ORI ty = P.check3Equal ty
hasType' OrB ty = P.check3Equal ty
hasType' AndI ty = P.check3Equal ty
hasType' AndB ty = P.check3Equal ty
hasType' XorI ty = P.check3Equal ty
hasType' XorB ty = P.check3Equal ty
hasType' NotI ty = P.check2Equal ty
hasType' NotB ty = P.check2Equal ty
hasType' CompareI ty = P.checkFirst2AndLast ty isBool
hasType' CompareS ty = P.checkFirst2AndLast ty isBool
hasType' CompareP ty = P.checkFirst2AndLast ty isBool
hasType' CompareTimeStamp ty = P.checkFirst2AndLast ty isBool
hasType' CompareMutez ty = P.checkFirst2AndLast ty isBool
hasType' CompareHash ty = P.checkFirst2AndLast ty isBool
-- Hacks make more exact later
hasType' EDivI (x :| [y, _]) = P.check2Equal (x :| [y])
hasType' EDivI _ = False
hasType' (Inst M.TRANSFER_TOKENS {}) (_ :| [_, _, _]) = True
hasType' (Inst M.UNIT {}) (_ :| []) = True
hasType' (Inst M.BALANCE {}) (_ :| []) = True
hasType' (Inst (M.IF_CONS _ _)) (bool :| rest)
  | empty == rest = False
  | otherwise = isBool bool && P.check2Equal (NonEmpty.fromList rest)
hasType' (Inst (M.IF _ _)) (bool :| rest)
  | empty == rest = False
  | otherwise = isBool bool && P.check2Equal (NonEmpty.fromList rest)
-- TODO: check this property
hasType' (Inst M.PAIR {}) (_ :| (_ : (_ : []))) = True
-- TODO: check this property
hasType' (Inst (M.CAR _ _)) (_ :| (_ : [])) = True
hasType' (Inst (M.CDR _ _)) (_ :| (_ : [])) = True
hasType' (Inst M.SENDER {}) (_ :| []) = True
hasType' Contract (_ :| [_]) = True
hasType' (Constant _v) ty
  | length ty == 1 = True
  | otherwise = False
hasType' _ ((Application List _) :| []) = True
hasType' (Inst (M.LSL _)) ((PrimTy (M.Ty M.TNat _)) :| [PrimTy (M.Ty M.TNat _), PrimTy (M.Ty M.TNat _)]) = True
hasType' (Inst (M.LSL _)) _ = False
hasType' (Inst (M.SHA256 _)) ((PrimTy (M.Ty M.TBytes _)) :| [PrimTy (M.Ty M.TBytes _)]) = True
hasType' (Inst (M.SHA256 _)) _ = False
hasType' (Inst (M.SHA512 _)) ((PrimTy (M.Ty M.TBytes _)) :| [PrimTy (M.Ty M.TBytes _)]) = True
hasType' (Inst (M.SHA512 _)) _ = False
hasType' (Inst (M.NOW _)) (PrimTy (M.Ty M.TTimestamp _) :| []) = True
hasType' (Inst (M.NOW _)) _ = False
hasType' (Inst (M.SET_DELEGATE _)) ((Application Types.Option (PrimTy (M.Ty M.TKeyHash _) :| [])) :| [PrimTy (M.Ty M.TOperation _)]) = True
hasType' (Inst (M.SET_DELEGATE _)) _ = False
-- do something nicer here
hasType' x ty = Prelude.error ("unsupported: " <> Juvix.Library.show x <> " :: " <> Juvix.Library.show ty)

-- | Return the arity of a raw Michelson value.
arityRaw :: RawPrimVal -> Natural
arityRaw (Inst inst) = fromIntegral (Instructions.toNumArgs inst)
arityRaw (Constant _) = 0
arityRaw prim =
  Run.instructionOf prim (Untyped.Ty Untyped.TUnit DSLU.blank)
    |> Instructions.toNewPrimErr
    |> arityRaw

-- | Datatype that is used for describing errors during the application process.
data ApplyError primTy
  = CompilationError CompilationError
  | ReturnTypeNotPrimitive (ErasedAnn.Type primTy)

instance Show primTy => Show (ApplyError primTy) where
  show (CompilationError perr) = Prelude.show perr
  show (ReturnTypeNotPrimitive ty) =
    "not a primitive type:\n\t" <> Prelude.show ty

type instance PP.Ann (ApplyError _) = HR.PPAnn

instance
  (PP.PrettySyntax primTy, HR.ToPPAnn (PP.Ann primTy)) =>
  PP.PrettyText (ApplyError primTy)
  where
  prettyT = \case
    CompilationError e -> HR.toPPAnn <$> PP.prettyT e
    ReturnTypeNotPrimitive ty ->
      PP.sepIndent'
        [ (False, "Not a primitive type:"),
          (True, PP.pretty0 ty)
        ]

data TyAppError
  = TooManyPrimArguments TypeTake (NonEmpty TypeTake)
  deriving (Eq, Show)

type instance PP.Ann TyAppError = HR.PPAnn

-- | Instance for types.
instance Core.CanPrimApply Core.Star RawPrimTy where
  type PrimApplyError RawPrimTy = TyAppError

  primArity = Run.lengthType

  primApply fun@(App.Take {type', term}) args =
    foldlM ap1 (type', term) args
    where
      ap1 (Core.PrimType tys0, term) (App.Take {term = arg})
        | (_ :| ty : tys) <- tys0 =
          Right (Core.PrimType (ty :| tys), ap1' term arg)
        | otherwise =
          Left $ TooManyPrimArguments fun args
      ap1' (Application fun args) arg = Application fun (args <> pure arg)
      ap1' fun arg = Application fun (pure arg)

instance Core.CanPrimApply RawPrimTy RawPrimVal where
  type PrimApplyError RawPrimVal = ApplyError PrimTyHR
  primArity = arityRaw
  primApply fun args =
    case compd >>= Interpreter.dummyInterpret of
      Right x -> do
        ty <-
          first CompilationError $
            Run.toPrimTypeEither $ ErasedAnn.type' newTerm
        pure (Core.PrimType ty, Constant x)
      Left err -> Left $ CompilationError err
    where
      fun' = takeToTerm fun
      args' = takeToTerm <$> toList args
      newTerm = Run.applyPrimOnArgs fun' args'
      -- TODO ∷ do something with the logs!?
      (compd, _log) = Compilation.compileExpr newTerm

-- | Translate a 'Return' into a 'RawTerm'.
takeToTerm :: Take -> Term
takeToTerm (App.Take {type', term}) =
  Ann
    { usage = Usage.SAny, -- TODO: Is SAny correct here?
      type' = ErasedAnn.fromPrimTypeT type',
      term = ErasedAnn.Prim $ App.Return {retTerm = term, retType = type'}
    }

-- | Translate an 'Integer' to the Michelson backend.
integerToPrimVal :: Integer -> Maybe RawPrimVal
integerToPrimVal x
  | x >= toInteger (minBound @Int),
    x <= toInteger (maxBound @Int) =
    Just $ Constant $ M.ValueInt $ fromInteger x
  | otherwise =
    Nothing

-- | Turn a Michelson type into a 'PrimTy'.
primify :: Untyped.T -> RawPrimTy
primify t = PrimTy (Untyped.Ty t DSLU.blank)

-- | Michelson-specific low-level types available in Juvix.
builtinTypes :: P.Builtins RawPrimTy
builtinTypes =
  [ ("Michelson.unit-t", Untyped.TUnit),
    ("Michelson.key", Untyped.TKey),
    ("Michelson.signature", Untyped.TSignature),
    ("Michelson.chain-id", Untyped.TChainId),
    ("Michelson.int", Untyped.TInt),
    ("Michelson.nat", Untyped.TNat),
    ("Michelson.string", Untyped.TString),
    ("Michelson.bytes", Untyped.TBytes),
    ("Michelson.mutez", Untyped.TMutez),
    ("Michelson.bool", Untyped.TBool),
    ("Michelson.key-hash", Untyped.TKeyHash),
    ("Michelson.timestamp", Untyped.TTimestamp),
    ("Michelson.address", Untyped.TAddress),
    ("Michelson.operation", Untyped.TOperation)
  ]
    |> fmap (NameSymbol.fromSymbol Arr.*** primify)
    |> ( <>
           [ ("Michelson.list", Types.List),
             ("Michelson.lambda", Types.Lambda),
             ("Michelson.option", Types.Option),
             ("Michelson.set", Types.Set),
             ("Michelson.map", Types.Map),
             ("Michelson.big-map", Types.BigMap),
             ("Michelson.pair-ty", Types.Pair),
             ("Michelson.contract", Types.ContractT)
           ]
       )
    |> Map.fromList

-- | Michelson-specific low-level values available in Juvix.
builtinValues :: P.Builtins RawPrimVal
builtinValues =
  [ ("Michelson.add", AddI),
    ("Michelson.sub", SubI),
    ("Michelson.mul", MulI),
    ("Michelson.div", EDivI),
    ("Michelson.now", Inst (M.NOW DSLU.blank)),
    ("Michelson.cons", Inst (M.CONS DSLU.blank)),
    ("Michelson.car", Inst (M.CAR DSLU.blank DSLU.blank)),
    ("Michelson.cdr", Inst (M.CDR DSLU.blank DSLU.blank)),
    ("Michelson.some", Inst (M.SOME DSLU.blank DSLU.blank)),
    ("Michelson.sha256", Inst (M.SHA256 DSLU.blank)),
    ("Michelson.sha512", Inst (M.SHA512 DSLU.blank)),
    ("Michelson.source", Inst (M.SOURCE DSLU.blank)),
    ("Michelson.get", Inst (M.GET DSLU.blank)),
    ("Michelson.update", Inst (M.UPDATE DSLU.blank)),
    ("Michelson.size", SizeS),
    ("Michelson.blake2b", Inst (M.BLAKE2B DSLU.blank)),
    ("Michelson.abs", Inst (M.ABS DSLU.blank)),
    ("Michelson.now", Inst (M.NOW DSLU.blank)),
    ("Michelson.source", Inst (M.SOURCE DSLU.blank)),
    ("Michelson.sender", Inst (M.SENDER DSLU.blank)),
    ("Michelson.set-delegate", Inst (M.SET_DELEGATE DSLU.blank)),
    ("Michelson.transfer-tokens", Inst (M.TRANSFER_TOKENS DSLU.blank)),
    ("Michelson.compare", CompareI),
    ("Michelson.amount", Inst (M.AMOUNT DSLU.blank)),
    ("Michelson.balance", Inst (M.BALANCE DSLU.blank)),
    ("Michelson.hash-key", Inst (M.HASH_KEY DSLU.blank)),
    ("Michelson.transfer-tokens", Inst (M.TRANSFER_TOKENS DSLU.blank)),
    ("Michelson.and", AndI),
    ("Michelson.xor", XorI),
    ("Michelson.or", OrB),
    ("Michelson.mem", MemMap),
    ("Michelson.concat", Inst (M.CONCAT DSLU.blank)),
    ("Michelson.slice", Inst (M.SLICE DSLU.blank)),
    ("Michelson.lsl", Inst (M.LSL DSLU.blank)),
    ("Michelson.lsr", Inst (M.LSR DSLU.blank)),
    ("Michelson.fail-with", Inst M.FAILWITH),
    ("Michelson.self", Inst (M.SELF DSLU.blank DSLU.blank)),
    ("Michelson.unit", Inst (M.UNIT DSLU.blank DSLU.blank)),
    ("Michelson.nil", Nil),
    ("Michelson.cons", Cons),
    ("Michelson.none", None),
    ("Michelson.left", Left'),
    ("Michelson.right", Right'),
    ("Michelson.map", MapOp),
    ("Michelson.empty-set", EmptyS),
    ("Michelson.empty-map", EmptyM),
    ("Michelson.empty-big-map", EmptyBM),
    ("Michelson.address-to-contract", Contract),
    -- added symbols to not take values
    ("Michelson.if-builtin", Inst (M.IF [] [])),
    ("Michelson.if-none", Inst (M.IF_NONE [] [])),
    ("Michelson.pair", Inst (M.PAIR DSLU.blank DSLU.blank DSLU.blank DSLU.blank))
  ]
    |> fmap (first NameSymbol.fromSymbol)
    |> Map.fromList

-- | Parameters for the Michelson backend.
michelson :: P.Parameterisation RawPrimTy RawPrimVal
michelson =
  P.Parameterisation
    { hasType = \x y ->
        let ans = hasType x y
         in ans,
      builtinTypes,
      builtinValues,
      stringVal = Just . Constant . M.ValueString . M.unsafeMkMText, -- TODO ?
      intVal = integerToPrimVal,
      floatVal = const Nothing
    }

instance Eval.HasWeak RawPrimTy where weakBy' _ _ t = t

instance Eval.HasWeak RawPrimVal where weakBy' _ _ t = t

instance
  Monoid (Core.XVPrimTy ext RawPrimTy primVal) =>
  Eval.HasSubstValueType ext RawPrimTy primVal RawPrimTy
  where
  substValueTypeWith _ _ _ t = pure $ Core.VPrimTy t mempty

instance
  Monoid (Core.XPrimTy ext RawPrimTy primVal) =>
  Eval.HasPatSubstType ext RawPrimTy primVal RawPrimTy
  where
  patSubstType' _ _ t = pure $ Core.PrimTy t mempty

instance
  Monoid (Core.XPrim ext primTy RawPrimVal) =>
  Eval.HasPatSubstTerm ext primTy RawPrimVal RawPrimVal
  where
  patSubstTerm' _ _ t = pure $ Core.Prim t mempty
