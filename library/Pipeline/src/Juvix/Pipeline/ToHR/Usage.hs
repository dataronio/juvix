module Juvix.Pipeline.ToHR.Usage (transformUsage, transformGUsage) where

import qualified Juvix.Core.Base as Core
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Juvix.Pipeline.ToHR.Env
import qualified Juvix.Pipeline.ToHR.Sig.Extract as Sig
import Juvix.Pipeline.ToHR.Types
import qualified Juvix.Sexp as Sexp

-- | Retrieve usage from numeric atom
transformUsage ::
  ( Show primTy,
    Show primVal,
    HasThrowFF ext primTy primVal m,
    HasCoreSigs ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m Usage.T
transformUsage _ (Sexp.Atom Sexp.N {atomNum = i}) | i >= 0 = pure $ Usage.SNat $ fromInteger i
transformUsage q e = do
  o <- isSAny q e
  if o then pure Usage.SAny else throwFF $ NotAUsage e

transformGUsage ::
  ( Show primTy,
    Show primVal,
    HasThrowFF ext primTy primVal m,
    HasCoreSigs ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Maybe Sexp.T ->
  m Core.GlobalUsage
transformGUsage _ Nothing = pure Core.GSAny
transformGUsage _ (Just (Sexp.Atom Sexp.N {atomNum = 0})) = pure Core.GZero
transformGUsage q (Just e) = do
  o <- isSAny q e
  if o then pure Core.GSAny else throwFF $ NotAGUsage e

isSAny ::
  ( Show primTy,
    Show primVal,
    HasCoreSigs ext primTy primVal m,
    HasThrowFF ext primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m Bool
isSAny q e = (== Just SAnyS) <$> Sig.getSpecialSig q e
