{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Core.Erased.Ann.Conversion
  ( irToErasedAnn,
    toRaw,
    toRawTerm,
    toRawType,
    primToRaw,
    CompConstraints',
    CompConstraints,
    returnToTerm,
    takeToTerm,
    argToTerm,
    argToType,
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
    HasReader "globals" (Typed.GlobalsT IR.T IR.T primTy primVal) m
  )

type CompConstraints primTy primVal compErr m =
  ( CompConstraints' primTy primVal compErr m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Types.CanPrimApply Types.Star primTy,
    Types.CanPrimApply primTy primVal,
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  )

irToHRMapPrim ::
  Erasure.MapPrim (Types.TypedPrim ty val) (E.Prim ty val) ty' val'
irToHRMapPrim ns = lookupRet
  where
    lookupArg (App.BoundArg i) =
      atMay ns (fromIntegral i) |> maybe (error i) (pure . App.VarArg)
    lookupArg (App.FreeArg x) = pure $ App.VarArg x
    lookupArg (App.TermArg ret) = App.TermArg <$> lookupRet ret

    lookupRet (App.Return {..}) = pure $ App.Return {..}
    lookupRet (App.Cont {args, ..}) = do
      args <- traverse lookupArg args
      pure $ App.Cont {..}

    error i = Left $ Erasure.InternalError $ "unknown de Bruijn index " <> show i

irToErasedAnn ::
  forall err ty val m.
  ( CompConstraints ty val err m,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      ty
      (Types.TypedPrim ty val)
      ty
  ) =>
  IR.Term ty val ->
  Usage.T ->
  IR.Term ty val ->
  m (ErasedAnn.AnnTermT ty val)
irToErasedAnn term usage ty = do
  (term, _) <- typecheckErase' term usage ty
  pure $ convertTerm term usage

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckEval ::
  ( CompConstraints primTy primVal compErr m,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Types.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.Term IR.T primTy primVal ->
  Usage.T ->
  Typed.ValueT IR.T primTy primVal ->
  m (Typed.ValueT IR.T primTy primVal)
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
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Types.TypedPrim primTy primVal)
      primTy
  ) =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Term primTy primVal ->
  m (Erasure.TermT primTy primVal, Typed.ValueT IR.T primTy primVal)
typecheckErase' term usage ty = do
  -- FIXME: allow any universe!
  ty <- typecheckEval ty (Usage.SNat 0) (IR.VStar $ Core.U 0)
  term <- typecheckErase term usage ty
  pure (term, ty)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ::
  ( CompConstraints primTy primVal compErr m,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Types.TypedPrim primTy primVal)
      primTy
  ) =>
  IR.Term primTy primVal ->
  Usage.T ->
  Typed.ValueT IR.T primTy primVal ->
  m (Erasure.TermT primTy primVal)
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- Typecheck & return accordingly.
  case IR.typeTerm param term (Typed.Annotation usage ty)
    |> IR.execTC globals
    |> fst of
    Right tyTerm -> do
      case Erasure.erase irToHRMapPrim irToHRMapPrim tyTerm usage of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError err)

toRaw :: ErasedAnn.AnnTermT ty val -> ErasedAnn.AnnTerm ty val
toRaw (ErasedAnn.Ann {term, type', ..}) =
  ErasedAnn.Ann {term = toRawTerm term, type' = toRawType type', ..}

toRawTerm :: ErasedAnn.TermT ty val -> ErasedAnn.Term ty val
toRawTerm (ErasedAnn.Var x) = ErasedAnn.Var x
toRawTerm (ErasedAnn.Prim p) = primToRaw p
toRawTerm (ErasedAnn.LamM {..}) = ErasedAnn.LamM {body = toRaw body, ..}
toRawTerm (ErasedAnn.PairM l r) = ErasedAnn.PairM (toRaw l) (toRaw r)
toRawTerm (ErasedAnn.CatProductIntroM l r) =
  ErasedAnn.CatProductIntroM (toRaw l) (toRaw r)
toRawTerm (ErasedAnn.CatProductElimLeftM a t) =
  ErasedAnn.CatProductElimLeftM (toRaw a) (toRaw t)
toRawTerm (ErasedAnn.CatProductElimRightM a t) =
  ErasedAnn.CatProductElimRightM (toRaw a) (toRaw t)
toRawTerm (ErasedAnn.CatCoproductIntroLeftM t) =
  ErasedAnn.CatCoproductIntroLeftM (toRaw t)
toRawTerm (ErasedAnn.CatCoproductIntroRightM t) =
  ErasedAnn.CatCoproductIntroRightM (toRaw t)
toRawTerm (ErasedAnn.CatCoproductElimM a b cp l r) =
  ErasedAnn.CatCoproductElimM (toRaw a) (toRaw b) (toRaw cp) (toRaw l) (toRaw r)
toRawTerm ErasedAnn.UnitM = ErasedAnn.UnitM
toRawTerm (ErasedAnn.AppM f xs) = ErasedAnn.AppM (toRaw f) (toRaw <$> xs)

toRawType :: HasCallStack => ErasedAnn.TypeT ty -> ErasedAnn.Type ty
toRawType (ErasedAnn.SymT x) = ErasedAnn.SymT x
toRawType (ErasedAnn.Star ℓ) = ErasedAnn.Star ℓ
toRawType (ErasedAnn.PrimTy t) =
  case t of
    App.Return {retTerm} -> ErasedAnn.PrimTy retTerm
    App.Cont {} -> notImplemented
-- FIXME for this we need type application in ErasedAnn.Type
toRawType (ErasedAnn.Pi π a b) = ErasedAnn.Pi π (toRawType a) (toRawType b)
toRawType (ErasedAnn.Sig π a b) = ErasedAnn.Sig π (toRawType a) (toRawType b)
toRawType (ErasedAnn.CatProduct a b) =
  ErasedAnn.CatProduct (toRawType a) (toRawType b)
toRawType (ErasedAnn.CatCoproduct a b) =
  ErasedAnn.CatCoproduct (toRawType a) (toRawType b)
toRawType ErasedAnn.UnitTy = ErasedAnn.UnitTy

primToRaw :: ErasedAnn.Prim ty val -> ErasedAnn.Term ty val
primToRaw (App.Return {retTerm}) = ErasedAnn.Prim retTerm
primToRaw (App.Cont {fun, args}) =
  ErasedAnn.AppM (takeToTerm fun) (argsToTerms (App.type' fun) args)
  where
    argsToTerms ts xs = go (toList ts) xs
    go _ [] = []
    go (_ : ts) (App.TermArg ret : as) =
      returnToTerm ret : go ts as
    go (t : ts) (App.VarArg x : as) =
      varTerm t x : go ts as
    go [] (_ : _) =
      -- a well typed application can't have more arguments than arrows
      panic "Erased.Ann.Conversion: argsToTerms: too many arguments"

    varTerm t x =
      ErasedAnn.Ann
        { usage = Usage.SAny, -- FIXME should usages even exist after erasure?
          type' = ErasedAnn.PrimTy t,
          term = ErasedAnn.Var x
        }

-- | Promotes a 'Return' to an 'AnnTerm'. Assumes that its input is well-typed!
returnToTerm ::
  forall k (ext :: k) primTy primVal.
  App.ParamVar ext ~ NameSymbol.T =>
  App.Return' ext (Types.PrimType primTy) primVal ->
  AnnTerm primTy primVal
returnToTerm (App.Return ty term) =
  ErasedAnn.AnnAny
    { typeA = ErasedAnn.fromPrimType ty,
      termA = ErasedAnn.Prim term
    }
returnToTerm (App.Cont {fun, args}) =
  ErasedAnn.AnnAny {typeA, termA}
  where
    App.Take {type' = type0@(Types.PrimType (toList -> tys)), term = head'} = fun
    typeA =
      case drop (length args) tys of
        [] -> panic "Erased.Ann.Conversion.returnToTerm: too many arguments"
        t : ts -> ErasedAnn.fromPrimType $ Types.PrimType $ t :| ts
    termA = ErasedAnn.AppM head args'
    head =
      ErasedAnn.AnnAny
        { typeA = ErasedAnn.fromPrimType type0,
          termA = ErasedAnn.Prim head'
        }
    args' = zipWith makeArg tys args
      where
        makeArg ty arg =
          ErasedAnn.AnnAny
            { typeA = ErasedAnn.PrimTy ty,
              termA = argToTerm arg
            }

argToTerm ::
  App.ParamVar ext ~ NameSymbol.T =>
  App.Arg' ext (Types.PrimType primTy) primVal ->
  Term primTy primVal
argToTerm (App.VarArg x) = ErasedAnn.Var x
argToTerm (App.TermArg t) = ErasedAnn.term $ returnToTerm t

argToType ::
  App.ParamVar ext ~ NonEmpty Symbol =>
  App.Arg' ext ty term ->
  Type (App.Return' ext ty term)
argToType = \case
  App.VarArg x -> SymT x
  App.TermArg ty -> PrimTy ty

takeToTerm :: App.Take (Types.PrimType primTy) primVal -> AnnTerm primTy primVal
takeToTerm App.Take {usage, type', term} =
  ErasedAnn.Ann
    { usage,
      type' = ErasedAnn.fromPrimType type',
      term = ErasedAnn.Prim term
    }

free :: forall primTy primVal. E.Term primTy primVal -> [NameSymbol.T]
free = Erased.free . E.eraseAnn

-- | Take a typed term and some usage, and annotate the return term with that usage
convertTerm :: E.TermT primTy primVal -> Usage.T -> AnnTermT primTy primVal
convertTerm term usage =
  let ty = E.getType term
      ty' = convertType ty
   in case term of
        E.Var sym _ -> Ann usage ty' (Var sym)
        E.Prim p _ -> Ann usage ty' $ Prim $ App.castReturn p
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

convertType :: E.TypeT primTy -> TypeT primTy
convertType ty =
  case ty of
    E.Star u -> Star u
    E.SymT s -> SymT s
    E.PrimTy p -> PrimTy $ App.castReturn p
    E.Pi u a r -> Pi u (convertType a) (convertType r)
    E.Sig u a b -> Sig u (convertType a) (convertType b)
    E.CatProduct a b -> CatProduct (convertType a) (convertType b)
    E.CatCoproduct a b -> CatCoproduct (convertType a) (convertType b)
    E.UnitTy -> UnitTy
