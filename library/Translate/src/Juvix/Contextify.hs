module Juvix.Contextify (fullyContextify, contextify, op, ResolveErr (..), PathError, resolveOpens, runM) where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify.Environment as Environment
import qualified Juvix.Contextify.Passes as Passes
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as ResolveOpen
import qualified Juvix.Contextify.ToContext.Sexp as ContextSexp
import qualified Juvix.Contextify.ToContext.Types as Contextify
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

type RunM =
  ExceptT Context.PathError IO

newtype M a = M (RunM a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasThrow "left" Context.PathError) via MonadError RunM

data ResolveErr
  = Path Context.PathError
  | Resolve ResolveOpen.Error
  | PassErr Environment.ErrorS
  deriving (Show, Eq)

type PathError t = Either Context.PathError t

--------------------------------------------------------------------------------
-- Main functionality
--------------------------------------------------------------------------------

op ::
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Either ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
op names = do
  context <- fullyContextify names
  case context of
    Left err -> pure $ Left err
    Right ctx -> do
      (newCtx, _) <- -- only Passes.resolveModule requires IO in these passes
        Environment.runMIO $
          Passes.resolveModule ctx
            >>= Passes.inifixSoloPass
      case newCtx of
        Left err -> pure $ Left $ PassErr err
        Right t -> pure (Right t)

-- | @fullyContextify@ runs @contextifyS@ along while running the
-- algorithm that resolves the opens to the modules in which they
-- should come from
fullyContextify ::
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Either ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
fullyContextify ts = do
  cont <- contextify ts
  case cont of
    Left e -> pure $ Left $ Path e
    Right x -> do
      addedOpens <- uncurry ResolveOpen.run x
      case addedOpens of
        Left e -> pure $ Left $ Resolve e
        Right x ->
          pure $ Right x

contextify ::
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (PathError (Contextify.ContextSexp, [ResolveOpen.PreQualified]))
contextify t@((sym, _) :| _) = do
  emptyCtx <- Context.empty sym
  runM $
    foldM resolveOpens (emptyCtx, []) (addTop <$> t)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- we get the opens
resolveOpens ::
  (MonadIO m, HasThrow "left" Context.PathError m) =>
  (Contextify.ContextSexp, [ResolveOpen.PreQualified]) ->
  (Context.NameSymbol, [Sexp.T]) ->
  m (Contextify.ContextSexp, [ResolveOpen.PreQualified])
resolveOpens (ctx', openList) (sym, xs) = do
  ctx <- ContextSexp.run ctx' (sym, xs)
  case ctx of
    Right Contextify.PS {ctxS, opensS, modsDefinedS} ->
      pure
        ( ctxS,
          ResolveOpen.Pre
            { opens = opensS,
              explicitModule = sym,
              implicitInner = modsDefinedS
            } :
          openList
        )
    Left err -> throw @"left" err

------------------------------------------------------------
-- Misc Helpers
------------------------------------------------------------

addTop :: Bifunctor p => p NameSymbol.T c -> p NameSymbol.T c
addTop = first (NameSymbol.cons Context.topLevelName)

runM :: M a -> IO (Either Context.PathError a)
runM (M a) = runExceptT a
