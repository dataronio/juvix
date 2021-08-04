module Juvix.Core.IR.Typechecker.API where

import Juvix.Library
import Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Typechecker.API.Types as Types
import qualified Juvix.Core.Typechecker.API.History as Hist

-- | This API is based on Idris's v1 proof state
-- | Check "Idris, a General Purpose Dependently Typed Programming Language"
-- | for more information

-- | `MetaProg` is parameterized so we can change implementation
-- | of Types and Terms if needed
-- | History-related combinators not available since it should be part
-- | of each modifying operation
class Monad m => MetaProg primTy primVal a b m | m -> a where
  -- | `proofGoal` returns the current sub-goal
  proofGoal     :: m b

  -- | `proofTerm` returns a term given the current proof
  proofTerm     :: m b

  -- | `proofContext` returns locally bound variables
  proofContext  :: m a

  -- | `check` typechecks an expression and returns its type
  check         :: a -> m b

  -- | `normalise` evaluates an expression and returns its normal form
  normalise     :: a -> m a

  -- | `unify` attempts to unify two expressions
  unify         :: a -> b -> m c

  -- | `newProof` returns a new empty local state
  newProof      :: m a

  -- | `newTerm` resets the proof state
  newTerm       :: m a

  -- | `focus` brings a proof to current state and sets all related internal state
  focus         :: a -> m b

  -- | `primUnify` attempts to unify primitive types
  primUnify     :: primTy1 -> primTy2 -> m b

  -- | `getState` returns the global context
  getState      :: m a

  -- | `putState` gives access to the global context
  putState      :: a -> m ()

  -- | `throwErr` relays error to internal structures
  throwErr      :: Types.Error primTy primVal -> m (Types.Error primTy primVal)

  -- | `trace` logs custom user operations
  trace         :: a -> m ()

  history       :: m (History primTy primVal)

  -- TODO: Check with Andy if we need more primitives

instance MetaProg (ProofState primTy primVal a b) where
  proofGoal = do
    g <- get @"proofGoals"
    pure g

  proofTerm = do
    p <- get @"proofTerm"
    pure p

  proofContext = do
    l <- get @"proofTerm"
    pure l

  check = undefined
  normalise = undefined
  unify = undefined
  newProof = undefined
  newTerm = undefined
  focus = undefined
  primUnify = undefined

  getState = do
    s <- get @"globals"
    pure s

  putState s = do
    set @"globals"
    set @"history" (Hist.GlobalSet, s)
    pure ()

  throwErr err = throw @"proofError" err

  trace op a = do
    set @"history" (Hist.UserOp op, a)
