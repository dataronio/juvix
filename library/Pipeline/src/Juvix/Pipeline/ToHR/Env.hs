module Juvix.Pipeline.ToHR.Env where

import qualified Juvix.Closure as Closure
import qualified Juvix.Context as Ctx
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (show)
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Pipeline.ToHR.Types
import qualified Juvix.Sexp as Sexp

data FFState ext primTy primVal = FFState
  { frontend :: Ctx.T Sexp.T Sexp.T Sexp.T,
    param :: P.Parameterisation primTy primVal,
    coreSigs :: CoreSigs ext primTy primVal,
    coreDefs :: CoreDefs ext primTy primVal,
    patVars :: HashMap.T Core.GlobalName Core.PatternVar,
    nextPatVar :: Core.PatternVar,
    ffOrder :: [NonEmpty NameSymbol.T],
    -- | closure represents the local names in scope that can't be
    -- registered by other means
    closure :: Closure.T
  }
  deriving (Generic)

type EnvAlias ext primTy primVal =
  ExceptT (Error ext primTy primVal) (State (FFState ext primTy primVal))

newtype Env ext primTy primVal a = Env {unEnv :: EnvAlias ext primTy primVal a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    (HasThrow "fromFrontendError" (Error ext primTy primVal))
    via MonadError (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "frontend" (Ctx.T Sexp.T Sexp.T Sexp.T),
      HasReader "frontend" (Ctx.T Sexp.T Sexp.T Sexp.T)
    )
    via ReaderField "frontend" (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "param" (P.Parameterisation primTy primVal),
      HasReader "param" (P.Parameterisation primTy primVal)
    )
    via ReaderField "param" (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "coreSigs" (CoreSigs ext primTy primVal),
      HasSink "coreSigs" (CoreSigs ext primTy primVal),
      HasState "coreSigs" (CoreSigs ext primTy primVal)
    )
    via StateField "coreSigs" (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "coreDefs" (CoreDefs ext primTy primVal),
      HasSink "coreDefs" (CoreDefs ext primTy primVal),
      HasState "coreDefs" (CoreDefs ext primTy primVal)
    )
    via StateField "coreDefs" (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "patVars" (HashMap.T Core.GlobalName Core.PatternVar),
      HasSink "patVars" (HashMap.T Core.GlobalName Core.PatternVar),
      HasState "patVars" (HashMap.T Core.GlobalName Core.PatternVar)
    )
    via StateField "patVars" (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "nextPatVar" Core.PatternVar,
      HasSink "nextPatVar" Core.PatternVar,
      HasState "nextPatVar" Core.PatternVar
    )
    via StateField "nextPatVar" (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "ffOrder" [NonEmpty NameSymbol.T],
      HasSink "ffOrder" [NonEmpty NameSymbol.T],
      HasState "ffOrder" [NonEmpty NameSymbol.T]
    )
    via StateField "ffOrder" (EnvAlias ext primTy primVal)
  -- we should move this to a reader at some point
  deriving
    ( HasSource "closure" Closure.T,
      HasSink "closure" Closure.T,
      HasState "closure" Closure.T
    )
    via StateField "closure" (EnvAlias ext primTy primVal)

type HasThrowFF ext primTy primVal =
  HasThrow "fromFrontendError" (Error ext primTy primVal)

type HasFrontend =
  HasReader "frontend" (Ctx.T Sexp.T Sexp.T Sexp.T)

type HasParam primTy primVal =
  HasReader "param" (P.Parameterisation primTy primVal)

type HasCoreSigs ext primTy primVal =
  HasState "coreSigs" (CoreSigs ext primTy primVal)

type HasCore ext primTy primVal =
  HasState "core" (CoreDefs ext primTy primVal)

type HasPatVars =
  HasState "patVars" (HashMap.T Core.GlobalName Core.PatternVar)

type HasNextPatVar =
  HasState "nextPatVar" Core.PatternVar

type HasOrder =
  HasState "ffOrder" [NonEmpty NameSymbol.T]

type HasClosure =
  HasState "closure" Closure.T

type ReduceEff ext primTy primVal m =
  ( HasThrowFF ext primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs ext primTy primVal m
  )

execEnv ::
  Ctx.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Env ext primTy primVal a ->
  Either (Error ext primTy primVal) a
execEnv ctx param env =
  fst $ runEnv ctx param env

evalEnv ::
  Ctx.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Env ext primTy primVal a ->
  FFState ext primTy primVal
evalEnv ctx param env =
  snd $ runEnv ctx param env

evalEnvEither ::
  Ctx.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Env ext primTy primVal a ->
  Either (Error ext primTy primVal) (FFState ext primTy primVal)
evalEnvEither ctx param env =
  case runEnv ctx param env of
    (Left err, _) -> Left err
    (Right _, state) -> Right state

runEnv ::
  Ctx.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Env ext primTy primVal a ->
  (Either (Error ext primTy primVal) a, FFState ext primTy primVal)
runEnv ctx param (Env env) =
  runIdentity $ runStateT (runExceptT env) initState
  where
    initState =
      FFState
        { frontend = ctx,
          param,
          coreSigs = mempty,
          coreDefs = mempty,
          patVars = mempty,
          nextPatVar = 0,
          ffOrder = [],
          closure = Closure.empty
        }

throwFF :: HasThrowFF ext primTy primVal m => Error ext primTy primVal -> m a
throwFF = throw @"fromFrontendError"
