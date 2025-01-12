{-# LANGUAGE RecordWildCards #-}

module Juvix.Pipeline.Compile
  ( isMain,
    typePrims,
    unsafeEvalGlobal,
  )
where

import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Pipeline.ToHR.Def as Def
import Juvix.Pipeline.ToHR.Types
import qualified Prelude as P

type Debug primTy primVal =
  ( Show primTy,
    Show primVal,
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal),
    Show (Param.Arg primTy),
    Show (Param.Arg primVal)
  )

isMain :: Core.RawGlobal ext primTy primVal -> Bool
isMain (Core.RawGFunction (Core.RawFunction (_ :| ["main"]) _ _ _)) = True
isMain _ = False

-- | Evaluate terms of a global definition
unsafeEvalGlobal ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.RawGlobal IR.T primTy primVal ->
  Core.Global IR.T IR.T primTy primVal
unsafeEvalGlobal globals g =
  case g of
    Core.RawGDatatype (Core.RawDatatype n pos a l cons) ->
      Core.GDatatype (Core.Datatype n pos (argEval globals <$> a) l (conEval globals <$> cons))
    Core.RawGDataCon (Core.RawDataCon n t d) ->
      Core.GDataCon $ Core.DataCon n (unsafeEval globals t) (funEval globals <$> d)
    Core.RawGFunction (Core.RawFunction n u t cs) ->
      Core.GFunction $
        Core.Function n u (unsafeEval globals t) (map (funClauseEval globals) cs)
    Core.RawGAbstract (Core.RawAbstract n u t) ->
      Core.GAbstract $ Core.Abstract n u (unsafeEval globals t)

-- | Type primitive values of a global definition
typePrims ::
  (Show ty, Show val, Param.CanPrimApply k ty) =>
  Core.RawGlobal IR.T ty val ->
  Core.RawGlobal IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
typePrims g =
  case g of
    Core.RawGDatatype (Core.RawDatatype n pos a l cons) ->
      Core.RawGDatatype (Core.RawDatatype n pos (argReturn <$> a) l (conReturn <$> cons))
    Core.RawGDataCon (Core.RawDataCon n t d) ->
      Core.RawGDataCon (Core.RawDataCon n (baseToReturn t) (funReturn <$> d))
    Core.RawGFunction (Core.RawFunction n u t cs) ->
      Core.RawGFunction (Core.RawFunction n u (baseToReturn t) (funClauseReturn <$> cs))
    Core.RawGAbstract (Core.RawAbstract n u t) ->
      Core.RawGAbstract (Core.RawAbstract n u (baseToReturn t))

argReturn ::
  Param.CanPrimApply k ty =>
  Core.RawDataArg IR.T ty val ->
  Core.RawDataArg IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
argReturn arg@Core.RawDataArg {rawArgType} =
  arg {Core.rawArgType = baseToReturn rawArgType}

argEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.RawDataArg IR.T primTy primVal ->
  Core.DataArg IR.T primTy primVal
argEval globals arg@Core.RawDataArg {rawArgName, rawArgUsage, rawArgType} =
  Core.DataArg rawArgName rawArgUsage (unsafeEval globals rawArgType)

conReturn ::
  Param.CanPrimApply k ty =>
  Core.RawDataCon IR.T ty val ->
  Core.RawDataCon IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
conReturn con@Core.RawDataCon {rawConType, rawConDef} =
  con {Core.rawConType = baseToReturn rawConType, Core.rawConDef = funReturn <$> rawConDef}

conEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.RawDataCon IR.T primTy primVal ->
  Core.DataCon IR.T IR.T primTy primVal
conEval globals con@Core.RawDataCon {rawConName, rawConType, rawConDef} =
  Core.DataCon rawConName (unsafeEval globals rawConType) (funEval globals <$> rawConDef)

funReturn ::
  Param.CanPrimApply k ty =>
  Core.RawFunction IR.T ty val ->
  Core.RawFunction IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
funReturn (Core.RawFunction name usage term clauses) =
  Core.RawFunction name usage (baseToReturn term) (funClauseReturn <$> clauses)

funEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.RawFunction IR.T primTy primVal ->
  Core.Function IR.T IR.T primTy primVal
funEval globals (Core.RawFunction name usage term clauses) =
  Core.Function name usage (unsafeEval globals term) (funClauseEval globals <$> clauses)

funClauseReturn ::
  Param.CanPrimApply k ty =>
  Core.RawFunClause IR.T ty val ->
  Core.RawFunClause IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
funClauseReturn (Core.RawFunClause tel patts term catchall) =
  Core.RawFunClause (telescopeReturn tel) (map pattEval patts) (baseToReturn term) catchall

funClauseEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.RawFunClause IR.T primTy primVal ->
  Core.FunClause IR.T IR.T primTy primVal
funClauseEval globals (Core.RawFunClause tel patts rhs catchall) =
  Core.FunClause
    (telescopeEval globals tel)
    patts
    rhs
    Nothing -- TODO:-- | @Δ ⊢ t@.  The type of the rhs under @clauseTel@.
    catchall
    Nothing --TODO

telescopeReturn ::
  Param.CanPrimApply k ty =>
  Core.RawTelescope IR.T ty val ->
  Core.RawTelescope IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
telescopeReturn = fmap f
  where
    f t@Core.RawTeleEle {rawTy, rawExtension} =
      t {Core.rawTy = baseToReturn rawTy, Core.rawExtension = rawExtension}

telescopeEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.RawTelescope IR.T primTy primVal ->
  Core.Telescope IR.T IR.T primTy primVal
telescopeEval globals ts = f <$> ts
  where
    f rawT@Core.RawTeleEle {..} =
      Core.TeleEle
        { name = rawName,
          usage = rawUsage,
          ty = unsafeEval globals rawTy,
          extension = rawExtension
        }

pattEval ::
  Param.CanPrimApply k ty =>
  IR.Pattern ty val ->
  IR.Pattern (Param.KindedType ty) (Param.TypedPrim ty val)
pattEval patt =
  case patt of
    IR.PCon n ps -> IR.PCon n (map pattEval ps)
    IR.PPair x y -> IR.PPair (pattEval x) (pattEval y)
    IR.PUnit -> IR.PUnit
    IR.PVar v -> IR.PVar v
    IR.PDot t -> IR.PDot (baseToReturn t)
    -- TODO
    IR.PPrim p -> panic "Primitive values in patterns are not yet implemented."

baseToReturn ::
  Param.CanPrimApply k ty =>
  Core.Term IR.T ty val ->
  Core.Term IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
baseToReturn t =
  case t of
    IR.Star u -> IR.Star u
    IR.PrimTy p ->
      IR.PrimTy (CoreApp.Return (Param.getPrimTypeKind p) p)
    IR.Prim p -> IR.Prim (CoreApp.Return (Param.PrimType $ notImplemented :| []) p)
    IR.Pi u x y -> IR.Pi u (baseToReturn x) (baseToReturn y)
    IR.Lam t -> IR.Lam (baseToReturn t)
    IR.Sig u x y -> IR.Sig u (baseToReturn x) (baseToReturn y)
    IR.Pair x y -> IR.Pair (baseToReturn x) (baseToReturn y)
    IR.CatProduct x y ->
      IR.CatProduct (baseToReturn x) (baseToReturn y)
    IR.CatCoproduct x y ->
      IR.CatCoproduct (baseToReturn x) (baseToReturn y)
    IR.CatProductIntro x y ->
      IR.CatProductIntro (baseToReturn x) (baseToReturn y)
    IR.CatProductElimLeft a x ->
      IR.CatProductElimLeft (baseToReturn a) (baseToReturn x)
    IR.CatProductElimRight a x ->
      IR.CatProductElimRight (baseToReturn a) (baseToReturn x)
    IR.CatCoproductIntroLeft x ->
      IR.CatCoproductIntroLeft (baseToReturn x)
    IR.CatCoproductIntroRight x ->
      IR.CatCoproductIntroRight (baseToReturn x)
    IR.CatCoproductElim a b cp x y ->
      IR.CatCoproductElim
        (baseToReturn a)
        (baseToReturn b)
        (baseToReturn cp)
        (baseToReturn x)
        (baseToReturn y)
    IR.Let u a b -> IR.Let u (elimToReturn a) (baseToReturn b)
    IR.UnitTy -> IR.UnitTy
    IR.Unit -> IR.Unit
    IR.Elim e -> IR.Elim (elimToReturn e)

elimToReturn ::
  Param.CanPrimApply k ty =>
  Core.Elim IR.T ty val ->
  Core.Elim IR.T (Param.KindedType ty) (Param.TypedPrim ty val)
elimToReturn e =
  case e of
    IR.Bound b -> IR.Bound b
    IR.Free n -> IR.Free n
    IR.App e t -> IR.App (elimToReturn e) (baseToReturn t)
    IR.Ann a b -> IR.Ann (baseToReturn a) (baseToReturn b)

unsafeEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  Core.RawGlobals IR.T primTy primVal ->
  Core.Term IR.T primTy primVal ->
  Core.Value IR.T primTy primVal
unsafeEval globals t = case IR.evalTerm (IR.rawLookupFun' globals) t of
  Right v -> v
  Left v -> panic $ "Failed to eval term: " <> show v
