{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Env where

import Control.Monad.Trans
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker.Error as Error
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)
import qualified Juvix.Library.Usage as Usage

data EnvCtx' ext primTy primVal = EnvCtx
  { globals :: Typed.GlobalsT IR.T IR.T primTy primVal
  }
  deriving (Generic)

type EnvCtx = EnvCtx' IR.T

type EnvAlias ext primTy primVal =
  ExceptT
    (Error.TypecheckError' IR.T ext primTy primVal)
    (State (EnvCtx' ext primTy primVal))

newtype EnvTypecheck' ext primTy primVal a
  = EnvTyp (EnvAlias ext primTy primVal a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasThrow "typecheckError" (Error.TypecheckError' IR.T ext primTy primVal)
    )
    via MonadError (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "globals" (Typed.GlobalsT IR.T IR.T primTy primVal),
      HasReader "globals" (Typed.GlobalsT IR.T IR.T primTy primVal)
    )
    via ReaderField "globals" (EnvAlias ext primTy primVal)

type EnvTypecheck = EnvTypecheck' IR.T

type HasGlobals primTy primVal = HasReader "globals" (Typed.GlobalsT IR.T IR.T primTy primVal)

type PrimSubstValue1 primTy primVal a =
  Eval.HasSubstValue IR.T primTy (Typed.Prim primTy primVal) a

type PrimSubstValue primTy primVal =
  ( PrimSubstValue1 primTy primVal primTy,
    PrimSubstValue1 primTy primVal (Typed.Prim primTy primVal)
  )

type PrimPatSubstTerm1 primTy primVal a =
  Eval.HasPatSubstTerm (OnlyExts.T IR.T) primTy (Typed.Prim primTy primVal) a

type PrimPatSubstTerm primTy primVal =
  ( PrimPatSubstTerm1 primTy primVal primTy,
    PrimPatSubstTerm1 primTy primVal (Typed.Prim primTy primVal)
  )

type CanTC' ext primTy primVal m =
  ( Error.HasThrowTC' IR.T ext primTy primVal m,
    HasGlobals primTy primVal m,
    PrimSubstValue primTy primVal,
    PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  )

type CanTC primTy primVal m = CanTC' IR.T primTy primVal m

exec ::
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (Error.TypecheckError primTy primVal) a, EnvCtx primTy primVal)
exec globals (EnvTyp env) =
  runState (runExceptT env) $ EnvCtx globals

type Context primTy primVal = [Typed.AnnotationT IR.T primTy primVal]

lookupCtx ::
  ( Eval.HasWeak primTy,
    Eval.HasWeak primVal
  ) =>
  Context primTy primVal ->
  Core.BoundVar ->
  Maybe (Typed.AnnotationT IR.T primTy primVal)
lookupCtx gam x = do
  Typed.Annotation π ty <- atMay gam (fromIntegral x)
  pure $ Typed.Annotation π (Eval.weakBy (x + 1) ty)

lookupGlobal ::
  (HasGlobals primTy primVal m, Error.HasThrowTC' IR.T ext primTy primVal m) =>
  Core.GlobalName ->
  m (Typed.ValueT IR.T primTy primVal, Core.GlobalUsage)
lookupGlobal x = do
  mdefn <- asks @"globals" $ HashMap.lookup x
  case mdefn of
    Just defn -> pure $ makeGAnn defn
    Nothing -> Error.throwTC (Error.UnboundGlobal x)
  where
    makeGAnn (Core.GDatatype (Core.Datatype {dataArgs, dataLevel})) =
      (foldr makePi (Core.VStar dataLevel mempty) dataArgs, Core.GZero)
    makeGAnn (Core.GDataCon (Core.DataCon {dataConType})) =
      (dataConType, Core.GSAny)
    makeGAnn (Core.GFunction (Core.Function {funType, funUsage})) =
      (funType, funUsage)
    makeGAnn (Core.GAbstract (Core.Abstract {absUsage, absType})) =
      (absType, absUsage)
    makePi (Core.DataArg {argUsage, argType}) res =
      Core.VPi argUsage argType res mempty

type UContext = [Usage.T]

type PatBinds primTy primVal = IntMap (Typed.AnnotationT IR.T primTy primVal)

type PatUsages = IntMap Usage.T

data InnerState' (ext :: Type) primTy primVal = InnerState
  { param :: Param.Parameterisation primTy primVal,
    patBinds :: PatBinds primTy primVal,
    bound :: Context primTy primVal
  }
  deriving (Generic)

type InnerState = InnerState' IR.T

type InnerTCAlias ext primTy primVal =
  StateT (InnerState' ext primTy primVal)

newtype InnerTCT ext primTy primVal m a
  = InnerTC (InnerTCAlias ext primTy primVal m a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "param" (Param.Parameterisation primTy primVal),
      HasReader "param" (Param.Parameterisation primTy primVal)
    )
    via ReaderField "param" (InnerTCAlias ext primTy primVal m)
  deriving
    ( HasSource "patBinds" (PatBinds primTy primVal),
      HasSink "patBinds" (PatBinds primTy primVal),
      HasState "patBinds" (PatBinds primTy primVal)
    )
    via StateField "patBinds" (InnerTCAlias ext primTy primVal m)
  deriving
    ( HasSource "bound" (Context primTy primVal),
      HasSink "bound" (Context primTy primVal),
      HasState "bound" (Context primTy primVal)
    )
    via StateField "bound" (InnerTCAlias ext primTy primVal m)

deriving via
  Lift (InnerTCAlias ext primTy primVal m)
  instance
    HasThrow "typecheckError" (Error.TypecheckError' IR.T ext primTy primVal) m =>
    HasThrow
      "typecheckError"
      (Error.TypecheckError' IR.T ext primTy primVal)
      (InnerTCT ext primTy primVal m)

deriving via
  Lift (InnerTCAlias ext primTy primVal m)
  instance
    HasSource "globals" (Typed.GlobalsT IR.T IR.T primTy primVal) m =>
    HasSource
      "globals"
      (Typed.GlobalsT IR.T IR.T primTy primVal)
      (InnerTCT ext primTy primVal m)

deriving via
  Lift (InnerTCAlias ext primTy primVal m)
  instance
    HasReader "globals" (Typed.GlobalsT IR.T IR.T primTy primVal) m =>
    HasReader
      "globals"
      (Typed.GlobalsT IR.T IR.T primTy primVal)
      (InnerTCT ext primTy primVal m)

type InnerTC' ext primTy primVal =
  InnerTCT ext primTy primVal (EnvTypecheck' ext primTy primVal)

type InnerTC primTy primVal = InnerTC' IR.T primTy primVal

type HasParam primTy primVal =
  HasReader "param" (Param.Parameterisation primTy primVal)

type HasPatBinds primTy primVal = HasState "patBinds" (PatBinds primTy primVal)

type HasBound primTy primVal = HasState "bound" (Context primTy primVal)

type CanInnerTC' ext primTy primVal m =
  ( CanTC' ext primTy primVal m,
    HasParam primTy primVal m,
    HasPatBinds primTy primVal m,
    HasBound primTy primVal m
  )

type CanInnerTC primTy primVal m = CanInnerTC' IR.T primTy primVal m

execInner ::
  Monad m =>
  InnerTCT ext primTy primVal m a ->
  InnerState' ext primTy primVal ->
  m a
execInner (InnerTC m) = evalStateT m

execInner' ::
  InnerTCT ext primTy primVal m a ->
  InnerState' ext primTy primVal ->
  m (a, InnerState' ext primTy primVal)
execInner' (InnerTC m) = runStateT m

-- | A map of global names to signatures
type Signature ext primTy primVal =
  Map.Map Core.GlobalName (SigDef ext primTy primVal)

type InnerTCSig ext primTy primVal =
  StateT (Signature ext primTy primVal)

-- | Signatures of functions, constructors, and datatypes
data SigDef ext primTy primVal
  = -- | function constant to its type, clauses
    FunSig
      (Core.Value ext primTy primVal)
      ( Either
          (NonEmpty (Core.RawFunClause ext primTy primVal))
          (NonEmpty (Core.FunClause ext ext primTy primVal))
      )
  | -- | constructor constant to its type
    ConSig (Core.Value ext primTy primVal)
  | -- | data type constant to # parameters, positivity of parameters, type
    DataSig Int [Core.Pos] (Core.Value ext primTy primVal)

-- Return type of all type-checking functions.
-- state monad for global signature
newtype TypeCheck ext primTy primVal m a
  = TypeCheck (InnerTCSig ext primTy primVal m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)
  deriving
    ( HasState "typeSigs" (Signature ext primTy primVal),
      HasSink "typeSigs" (Signature ext primTy primVal),
      HasSource "typeSigs" (Signature ext primTy primVal)
    )
    via MonadState (InnerTCSig ext primTy primVal m)
