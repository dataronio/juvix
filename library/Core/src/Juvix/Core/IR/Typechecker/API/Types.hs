{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}

module Juvix.Core.IR.Typechecker.API.Types where

import Juvix.Library
import Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Typechecker.API.History as Hist
-- import qualifief Juvix.Core.Unification as Uni

-- | This API is based on Idris's v1 proof state
-- | Check "Idris, a General Purpose Dependently Typed Programming Language"
-- | for more information

data Error primTy primVal a
  = UnificationErr   a -- Uni.UnificationErr
  | HolesErr         a -- Uni.HolesErr
  | LocalContextErr  a -- Uni.TermContextErr
  | GlobalContextErr a -- Core.GlobalErr
  | MetaProgErr      MetaProgErr
  | PrimErr          (Core.PrimErr primTy primVal)

 -- `a` is a placeholder for the actual types
data ProofState primTy primVal a = Env
  { -- captures all changes to proof term
    history        :: Hist.History a b,

    -- unsolved unification problems
    -- requires unification
    -- TODO: check with Andy how the unification goal works
    proofGoal      :: a -- Uni.UnificationGoal primTy primVal,

    -- current proof
    -- TODO: check with Andy how to deal with bidirectional typecheking here
    proofTerm      :: Core.Term primTy primVal,

     -- global context with all types
    globals        :: Core.RawGlobal primTy primVal,

    -- local context of `proofTerm`
    -- requires unification
    locals         :: a -- Uni.TermConxtext primTy primVal,

    -- queue of current holes and guesses
    -- requires unification
    holes          :: a -- Uni.Holes primTy primVal
  }
  deriving (Generic)

type ProofStateA primTy primVal2 =
  ExceptT
    (Error primTy primVal)
    (State (ProofState primTy primVal))

newtype ProofStateT primTy primVal a b
  = ProofStateT (ProofStateA primTy primVal a b)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState  "history" (History a b),
      HasSink   "history" (History a b),
      HasSource "history" (History a b)
    )
    via StateField "history" (ProofStateA primTy primVal a b)
  deriving
    ( HasState  "globals" (History primTy primVal),
      HasSink   "globals" (History primTy primVal),
      HasSource "globals" (History primTy primVal)
    )
    via StateField "history" (ProofStateA primTy primVal a b)

  deriving
    ( HasState "proofTerm" a,
      HasSink "proofTerm" a,
      HasSource "proofTerm" a
    )
    via StateField "proofTerm" (ProofStateA primTy primVal a b)
  deriving
    ( HasState "proofGoal" a,
      HasSink "proofGoal" a,
      HasSource "proofGoal" a
    )
    via StateField "" (ProofStateA primTy primVal a b)
  deriving
    ( HasState "locals" a,
      HasSink "locals" a,
      HasSource "locals" a
    )
    via StateField "" (ProofStateA primTy primVal a b)
  deriving
    ( HasState "holes" a,
      HasSink "holes" a,
      HasSource "holes" a
    )
    via StateField "" (ProofStateA primTy primVal a b)
  deriving
    (HasThrow "proofError" (Error primTy1 primVal1 a))
    via MonadError (ProofStateA primTy primVal)

data MetaProgErr
  = MetaProgErr -- not sure what errors can happen here, but will leave a placeholder
