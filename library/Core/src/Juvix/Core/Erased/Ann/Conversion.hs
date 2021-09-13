{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Juvix.Core.Erased.Ann.Conversion
  ( irToErasedAnn,
    toRaw,
    CompConstraints',
    CompConstraints,
    returnToTerm,
    takeToTerm,
  )
where

import Data.List ((\\))
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.Erased.Algorithm as Erasure
import qualified Juvix.Core.Erased.Algorithm.Types as E
import qualified Juvix.Core.Erased.Ann.Prim as ErasedAnn
import Juvix.Core.Erased.Ann.Types
import qualified Juvix.Core.Erased.Ann.Types as ErasedAnn
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.IR.Typechecker as Typed
import qualified Juvix.Core.Types as Types
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

type CompConstraints' primTy primVal compErr m =
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (Core.Globals IR.T IR.T primTy (Types.TypedPrim primTy primVal)) m
  )

type CompConstraints primTy primVal compErr m =
  ( CompConstraints' primTy primVal compErr m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Types.CanApply primTy,
    Types.CanApply (Types.TypedPrim primTy primVal),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  )

constMapPrim :: Erasure.MapPrim a a ty val
constMapPrim _ = Right

lookupMapPrim ::
  Erasure.MapPrim
    (Types.TypedPrim ty val)
    (ErasedAnn.TypedPrim ty val)
    ty
    (Types.TypedPrim ty val)
lookupMapPrim _ (App.Return ty tm) = pure $ App.Return ty tm
lookupMapPrim ns (App.Cont f xs n) =
  App.Cont f <$> traverse lookupArg xs <*> pure n
  where
    lookupArg (App.BoundArg i) =
      atMay ns (fromIntegral i)
        |> maybe (error i) (pure . App.VarArg)
    lookupArg (App.FreeArg x) = pure $ App.VarArg x
    lookupArg (App.TermArg (App.Return ty term)) =
      pure $ App.TermArg (App.Return ty term)
    lookupArg (App.TermArg (App.Cont take args nat)) =
      notImplemented
    error i =
      Left $
        Erasure.InternalError $
          "unknown de Bruijn index " <> show i

irToErasedAnn ::
  forall err ty val m.
  ( CompConstraints ty val err m,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      ty
      (Types.TypedPrim ty val)
      ty
  ) =>
  IR.Term ty val ->
  Usage.T ->
  IR.Term ty val ->
  m (ErasedAnn.AnnTerm ty (ErasedAnn.TypedPrim ty val))
irToErasedAnn term usage ty = do
  -- FIXME: allow any universe!
  (term, _) <- typecheckErase' term usage ty
  pure $ convertTerm term usage

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckEval ::
  ( CompConstraints primTy primVal compErr m,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Types.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.Term IR.T primTy primVal ->
  Usage.T ->
  IR.Value primTy (Types.TypedPrim primTy primVal) ->
  m (IR.Value primTy (Types.TypedPrim primTy primVal))
typecheckEval term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  case IR.typeTerm param term (Typed.Annotation usage ty)
    >>= IR.evalTC
    |> IR.execTC globals
    |> fst of
    Right value -> pure value
    Left err -> throw @"error" (Types.TypecheckerError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase' ::
  ( CompConstraints primTy primVal compErr m,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Types.TypedPrim primTy primVal)
      primTy
  ) =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Term primTy primVal ->
  m
    ( Erasure.Term primTy (ErasedAnn.TypedPrim primTy primVal),
      IR.Value primTy (Types.TypedPrim primTy primVal)
    )
typecheckErase' term usage ty = do
  ty <- typecheckEval ty (Usage.SNat 0) (IR.VStar 0)
  term <- typecheckErase term usage ty
  pure (term, ty)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ::
  ( CompConstraints primTy primVal compErr m,
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Types.TypedPrim primTy primVal)
      primTy
  ) =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Value primTy (Types.TypedPrim primTy primVal) ->
  m (Erasure.Term primTy (ErasedAnn.TypedPrim primTy primVal))
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- Typecheck & return accordingly.
  case IR.typeTerm param term (Typed.Annotation usage ty)
    |> IR.execTC globals
    |> fst of
    Right tyTerm -> do
      case Erasure.erase constMapPrim lookupMapPrim tyTerm usage of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError err)

toRaw :: (Show ty, Show val) => ErasedAnn.AnnTerm ty (ErasedAnn.TypedPrim ty val) -> ErasedAnn.AnnTerm ty val
toRaw t@ErasedAnn.Ann {term} = t {ErasedAnn.term = toRaw1 term}
  where
    toRaw1 (ErasedAnn.Var x) = ErasedAnn.Var x
    toRaw1 (ErasedAnn.Prim p) = primToRaw p
    toRaw1 ErasedAnn.LamM {..} = ErasedAnn.LamM {body = toRaw body, ..}
    toRaw1 (ErasedAnn.PairM l r) = ErasedAnn.PairM (toRaw l) (toRaw r)
    toRaw1 (ErasedAnn.CatProductIntroM l r) = ErasedAnn.CatProductIntroM (toRaw l) (toRaw r)
    toRaw1 (ErasedAnn.CatProductElimLeftM a t) = ErasedAnn.CatProductElimLeftM (toRaw a) (toRaw t)
    toRaw1 (ErasedAnn.CatProductElimRightM a t) = ErasedAnn.CatProductElimRightM (toRaw a) (toRaw t)
    toRaw1 (ErasedAnn.CatCoproductIntroLeftM t) = ErasedAnn.CatCoproductIntroLeftM (toRaw t)
    toRaw1 (ErasedAnn.CatCoproductIntroRightM t) = ErasedAnn.CatCoproductIntroRightM (toRaw t)
    toRaw1 (ErasedAnn.CatCoproductElimM a b cp l r) =
      ErasedAnn.CatCoproductElimM
        (toRaw a)
        (toRaw b)
        (toRaw cp)
        (toRaw l)
        (toRaw r)
    toRaw1 ErasedAnn.UnitM = ErasedAnn.UnitM
    toRaw1 (ErasedAnn.AppM f xs) = ErasedAnn.AppM (toRaw f) (toRaw <$> xs)
    primToRaw App.Return {retTerm} = ErasedAnn.Prim retTerm
    primToRaw App.Cont {fun, args} =
      ErasedAnn.AppM (takeToTerm fun) (argsToTerms (App.type' fun) args)

    argsToTerms ts xs = go (toList ts) xs
      where
        go _ [] = []
        go (_ : ts) (App.TermArg (App.Return ty term) : as) =
          returnToTerm (App.Return ty term) : go ts as
        go (_ : ts) (App.TermArg (App.Cont fun args numLeft) : as) =
          notImplemented
        go (t : ts) (App.VarArg x : as) =
          varTerm t x : go ts as
        go [] (_ : _) =
          -- a well typed application can't have more arguments than arrows
          undefined
        varTerm t x =
          ErasedAnn.Ann
            { usage = Usage.SAny, -- FIXME should usages even exist after erasure?
              type' = ErasedAnn.PrimTy t,
              term = ErasedAnn.Var x
            }

returnToTerm ::
  forall k (ext :: k) primTy primVal.
  Show primTy =>
  App.Return' ext (Types.PrimType primTy) primVal ->
  AnnTerm primTy primVal
returnToTerm (App.Return ty term) =
  ErasedAnn.Ann
    { usage = Usage.SAny, -- FIXME should usages even exist after erasure?
      type' = ErasedAnn.fromPrimType ty,
      term = ErasedAnn.Prim term
    }
returnToTerm (App.Cont take args nat) = notImplemented

takeToTerm :: (Show primTy) => App.Take (Types.PrimType primTy) primVal -> AnnTerm primTy primVal
takeToTerm App.Take {usage, type', term} =
  ErasedAnn.Ann
    { usage,
      type' = ErasedAnn.fromPrimType type',
      term = ErasedAnn.Prim term
    }

free :: forall primTy primVal. E.Term primTy primVal -> [NameSymbol.T]
free = Erased.free . E.eraseAnn

-- | Take a typed term and some usage, and annotate the return term with that usage
convertTerm :: E.Term primTy primVal -> Usage.T -> AnnTerm primTy primVal
convertTerm term usage =
  let ty = E.getType term
      ty' = convertType ty
   in case term of
        E.Var sym _ -> Ann usage ty' (Var sym)
        E.Prim p _ -> Ann usage ty' (Prim p)
        E.Let sym bind body (bindTy, _) ->
          -- Calculate captures.
          let captures = Erased.free (Erased.Lam sym (E.eraseAnn body))
              -- TODO: Is this the right usage?
              bind' = convertTerm bind usage
              body' = convertTerm body usage
              bindTy' = convertType bindTy
              -- TODO: Eventually add `let` to Michelson, probably, instead of this conversion.
              lamTy = Pi usage bindTy' ty'
              lam = Ann usage lamTy (LamM captures [sym] body')
           in Ann usage ty' $ AppM lam [bind']
        E.Lam sym body _ ->
          -- TODO: Is this the right usage?
          case convertTerm body usage of
            -- Combine nested lambdas into multi-argument function.
            Ann _ _ (LamM cap' arg' body') ->
              Ann usage ty' $ LamM (cap' \\ [sym]) (sym : arg') body'
            body' ->
              Ann usage ty' $ LamM (free term) [sym] body'
        E.Pair left right _ ->
          let left' = convertTerm left usage
              right' = convertTerm right usage
           in Ann usage ty' $ PairM left' right'
        E.CatProductIntro left right _ ->
          let left' = convertTerm left usage
              right' = convertTerm right usage
           in Ann usage ty' $ CatProductIntroM left' right'
        E.CatProductElimLeft a t _ ->
          let a' = convertTerm a usage
              t' = convertTerm t usage
           in Ann usage ty' $ CatProductElimLeftM a' t'
        E.CatProductElimRight a t _ ->
          let a' = convertTerm a usage
              t' = convertTerm t usage
           in Ann usage ty' $ CatProductElimRightM a' t'
        E.CatCoproductIntroLeft t _ ->
          let t' = convertTerm t usage
           in Ann usage ty' $ CatCoproductIntroLeftM t'
        E.CatCoproductIntroRight t _ ->
          let t' = convertTerm t usage
           in Ann usage ty' $ CatCoproductIntroRightM t'
        E.CatCoproductElim a b cp left right _ ->
          let a' = convertTerm a usage
              b' = convertTerm b usage
              cp' = convertTerm cp usage
              left' = convertTerm left usage
              right' = convertTerm right usage
           in Ann usage ty' $ CatCoproductElimM a' b' cp' left' right'
        E.Unit _ ->
          Ann usage ty' UnitM
        E.App f a _ ->
          case convertTerm f usage of
            -- Combine nested application into multi-argument application.
            Ann _ _ (AppM f' as) ->
              Ann usage ty' $ AppM f' (as <> [a'])
            f' ->
              Ann usage ty' $ AppM f' [a']
          where
            a' = convertTerm a usage

convertType :: E.Type primTy -> Type primTy
convertType ty =
  case ty of
    E.Star u -> Star u
    E.SymT s -> SymT s
    E.PrimTy p -> PrimTy p
    E.Pi u a r -> Pi u (convertType a) (convertType r)
    E.Sig u a b -> Sig u (convertType a) (convertType b)
    E.CatProduct a b -> CatProduct (convertType a) (convertType b)
    E.CatCoproduct a b -> CatCoproduct (convertType a) (convertType b)
    E.UnitTy -> UnitTy
