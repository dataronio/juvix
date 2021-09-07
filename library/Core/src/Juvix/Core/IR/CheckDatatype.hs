-- | Datatype declarations are typechecked here. Usages are passed along.
module Juvix.Core.IR.CheckDatatype
  ( typeCheckAllCons,
    checkDataType,
  )
where

import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.CheckTerm
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.IR.Typechecker.Error as Error
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

-- | check all constructors of a datatype
typeCheckAllCons ::
  ( Error.HasThrowTC' IR.T extT primTy primVal m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Show (Core.Pattern extT primTy primVal),
    Show (Core.RawGlobal extT primTy primVal),
    Show (Core.Pattern extT primTy (Typed.Prim primTy primVal)),
    Env.CanTC' extT primTy primVal m,
    Param.CanApply (Typed.Prim primTy primVal),
    Eval.NoExtensions extT primTy (Typed.Prim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Eval.CanEval extT IR.T primTy primVal,
    Eval.EvalPatSubst IR.T primTy primVal,
    Eval.EvalPatSubst IR.T primTy (Typed.Prim primTy primVal),
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  -- |
  Core.Telescope IR.T extT primTy primVal ->
  -- | Positivity of its parameters
  [Core.Pos] ->
  Core.RawTelescope extT primTy primVal ->
  -- | a hashmap of global names and their info
  Typed.GlobalsT IR.T extT primTy primVal ->
  -- | The constructors to be checked
  [Core.RawDataCon extT primTy primVal] ->
  Env.TypeCheck IR.T primTy primVal m [Core.RawGlobal extT primTy primVal]
typeCheckAllCons param tel pos rtel globals =
  mapM (typeCheckConstructor param tel pos rtel globals)

typeCheckConstructor ::
  forall extT primTy primVal m.
  ( Error.HasThrowTC' IR.T extT primTy primVal m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Show (Core.Pattern extT primTy primVal),
    Show (Core.Pattern extT primTy (Typed.Prim primTy primVal)),
    Show (Core.RawGlobal extT primTy primVal),
    Eval.EvalPatSubst extT primTy primVal,
    Env.CanTC' extT primTy primVal m,
    Param.CanApply (Typed.Prim primTy primVal),
    Eval.NoExtensions extT primTy (Typed.Prim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Eval.CanEval extT IR.T primTy primVal,
    Eval.EvalPatSubst IR.T primTy primVal,
    Eval.EvalPatSubst IR.T primTy (Typed.Prim primTy primVal),
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  Core.Telescope IR.T extT primTy primVal ->
  -- | Positivity of its parameters
  [Core.Pos] ->
  Core.RawTelescope extT primTy primVal ->
  -- | a hashmap of global names and their info
  Typed.GlobalsT IR.T extT primTy primVal ->
  -- | The constructor to be checked
  Core.RawDataCon extT primTy primVal ->
  Env.TypeCheck IR.T primTy primVal m (Core.RawGlobal extT primTy primVal)
typeCheckConstructor param tel _lpos rtel globals con = do
  let cname = Core.rawConName con
      conTy = Core.rawConType con
      (name, t) = teleToType rtel conTy
  -- FIXME replace 'lift' with whatever capability does
  typechecked <- lift $ typeTerm param t (Typed.Annotation mempty (IR.VStar 0))
  evaled <- lift $ liftEval $ Eval.evalTerm (Eval.lookupFun @IR.T globals) typechecked
  checkConType tel param evaled
  let (_, target) = typeToTele (name, t)
  -- FIXME replace 'lift'
  lift $ checkDeclared cname rtel target
  -- put (addSig sig n (ConSig vt))
  return $ Core.RawGDataCon con

teleToType ::
  Core.RawTelescope extT primTy primVal ->
  Core.Term extT primTy primVal ->
  (Maybe Core.GlobalName, Core.Term extT primTy primVal)
teleToType [] t = (Nothing, t)
teleToType (hd : tel) t2 =
  ( Just (Core.rawName hd),
    Core.Pi
      (Core.rawUsage hd)
      (Core.rawTy hd)
      (snd (teleToType tel t2))
      (Core.rawExtension hd)
  )

typeToTele ::
  (Maybe Core.GlobalName, Core.Term ext primTy primVal) ->
  (Core.RawTelescope ext primTy primVal, Core.Term ext primTy primVal)
typeToTele (n, t) = ttt (n, t) []
  where
    ttt ::
      (Maybe Core.GlobalName, Core.Term ext primTy primVal) ->
      Core.RawTelescope ext primTy primVal ->
      (Core.RawTelescope ext primTy primVal, Core.Term ext primTy primVal)
    ttt (Just n, Typed.Pi usage t' t2 ext) tel =
      ttt
        (Nothing, t2)
        ( tel
            <> [ Core.RawTeleEle
                   { rawName = n,
                     rawUsage = usage,
                     rawTy = t',
                     rawExtension = ext
                   }
               ]
        )
    ttt x tel = (tel, snd x)

-- | checkDataType checks the datatype by checking all arguments.
-- The data constructors are checked by another function.
checkDataType ::
  ( Eval.CanEval IR.T extT primTy primVal,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Core.ValueAll Eq IR.T primTy primVal,
    Core.NeutralAll Eq IR.T primTy primVal,
    Env.CanTC' extT primTy primVal m,
    Param.CanApply (Typed.Prim primTy primVal),
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  -- | an env that contains the parameters of the datatype
  Core.Telescope IR.T extT primTy primVal ->
  -- | name of the datatype
  Core.GlobalName ->
  Param.Parameterisation primTy primVal ->
  -- | the list of args to be checked.
  [Core.RawDataArg extT primTy primVal] ->
  m ()
checkDataType tel dtName param =
  mapM_ (checkDataTypeArg tel dtName param . Core.rawArgType)

-- | checkDataTypeArg checks an argument of the datatype
checkDataTypeArg ::
  ( Eval.CanEval IR.T extT primTy primVal,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Core.ValueAll Eq IR.T primTy primVal,
    Core.NeutralAll Eq IR.T primTy primVal,
    Env.CanTC' extT primTy primVal m,
    Param.CanApply (Typed.Prim primTy primVal),
    Eval.HasPatSubstTerm
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  -- | an env that contains the parameters of the datatype
  Core.Telescope IR.T extT primTy primVal ->
  -- | name of the datatype
  Core.GlobalName ->
  -- | targeted backend/parameterisation
  Param.Parameterisation primTy primVal ->
  -- | the arg to be checked.
  Core.Term extT primTy primVal ->
  m ()
checkDataTypeArg tel dtName param (Typed.Pi _ t1 t2 _) = do
  _ <- typeTerm param t1 (Typed.Annotation mempty (IR.VStar 0))
  checkDataTypeArg tel dtName param t2
checkDataTypeArg _ _ _ (Core.Star _ _) = return ()
-- the arg can only be of star type of function type
checkDataTypeArg _ _ _ arg = Error.throwTC $ Error.DatatypeError arg

-- | type check a constructor
checkConType ::
  ( Eval.CanEval IR.T extT primTy primVal,
    Eq primTy,
    Eq primVal,
    Core.ValueAll Eq IR.T primTy primVal,
    Core.NeutralAll Eq IR.T primTy primVal,
    Env.CanTC' extT primTy primVal m,
    Param.CanApply (Typed.Prim primTy primVal)
  ) =>
  -- | an env that contains the parameters of the datatype
  Core.Telescope IR.T extT primTy primVal ->
  Param.Parameterisation primTy primVal ->
  -- | the expression that is left to be checked.
  Core.Value IR.T primTy (Typed.Prim primTy primVal) ->
  Env.TypeCheck IR.T primTy primVal m ()
checkConType tel param e =
  case e of
    -- the constructor could be a function type
    Core.VPi _ _ t2 _ ->
      -- no need to check function argument because
      -- it's been type checked in typeCheckConstructor (?)
      checkConType tel param t2
    -- or a star type
    Core.VStar _ _ -> return ()
    -- a constructor cannot be any other type
    _ -> lift $ Error.throwTC $ Error.ConTypeError e

-- | check that the data type and the parameter arguments
-- are written down like declared in telescope
checkDeclared ::
  (HasThrow "typecheckError" (Error.TypecheckError' IR.T extT primTy primVal) m) =>
  Core.GlobalName ->
  Core.RawTelescope extT primTy primVal ->
  Core.Term extT primTy primVal ->
  m ()
checkDeclared name tel tg@(Core.Elim (Core.App (Core.Free (Core.Global n) _) term _) _) =
  if n == name
    then do
      checkParams tel term -- check parameters
    else Error.throwTC $ Error.DeclError tg name tel
checkDeclared name tel tg@(Core.Elim (Core.Free (Core.Global n) _) _) =
  if n == name && null tel
    then return ()
    else Error.throwTC $ Error.DeclError tg name tel
checkDeclared name tel tg =
  Error.throwTC $ Error.DeclError tg name tel

-- check parameters
checkParams ::
  (HasThrow "typecheckError" (Error.TypecheckError' IR.T extT primTy primVal) m) =>
  Core.RawTelescope extT primTy primVal ->
  Core.Term extT primTy primVal ->
  m ()
checkParams (hd : tl) para@(Core.Elim elim _) =
  let n = Core.rawName hd
   in case elim of
        Core.Free (Core.Global n') _ | n == n' -> return ()
        Core.App (Core.Free (Core.Global n') _) term _ | n == n' -> checkParams tl term
        _ -> Error.throwTC $ Error.ParamError n para
checkParams [] _ = return ()
checkParams (hd : _) exp =
  Error.throwTC $ Error.ParamError (Core.rawName hd) exp
