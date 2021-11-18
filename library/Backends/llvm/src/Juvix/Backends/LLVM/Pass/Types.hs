module Juvix.Backends.LLVM.Pass.Types where

import Juvix.Backends.LLVM.Primitive as Prim
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

--------------------------------------------------------------------------------
-- Closure Capture Forms
--------------------------------------------------------------------------------

data Capture = Capture
  { -- | @location@ represents where in the environment do we
    -- capture the value from.
    location :: CaptureFrom,
    -- | @slot@ represents the new slot that represents this
    -- captured value, for proper GEP generation.
    slot :: ArraySlot,
    -- | @capType@ represents the capture type of the captured
    -- argument
    capType :: ErasedAnn.Type Prim.PrimTy
  }
  deriving (Show)

----------------------------------------
-- Slot Location Storage Types
----------------------------------------

-- | @ArraySlot@ represents the slot in the array the node now is in
-- along with metadata associated with the slot.
data ArraySlot = Slot
  { -- | @previousName@ represents the old name in the source code,
    -- recorded for debugging purposes.
    previousName :: NameSymbol.T,
    -- | @newIndex@ represents the new slot location into the array.
    newIndex :: Index
  }
  deriving (Show)

-- | @Index@ is the index into the array
newtype Index = Index {num :: Natural} deriving (Show)

----------------------------------------
-- Storage Location and Indexing Types
----------------------------------------

data CaptureFrom
  = -- | @FromAmbientEnv@ represents the values that we capture having a
    -- proper name rather then being offsets into the environment.
    FromAmbientEnv NameSymbol.T
  | -- | @FromClosureEnv@ represents that the value that we are trying
    -- to store are offsets inside some environment, rather than a name
    -- that we can directly bind.
    FromClosureEnv IndexInto
  deriving (Show)

-- | @IndexInto@ represents the Index into an environment, along with
-- which environment it originates from.
data IndexInto = IndexInto
  { index :: Index,
    into :: FunctionEnvironment
  }
  deriving (Show)

-- | @FunctionEnvironment@ represents if the index into an array is
-- from the closure array/environment or the closure argument array/environment
data FunctionEnvironment
  = ClosureEnvironment
  | ArgumentEnvironemnt
  deriving (Show)

--------------------------------------------------------------------------------
-- New Core Form we will Process Over
--------------------------------------------------------------------------------

data Annotated term = Ann
  { usage :: Usage.T,
    type' :: ErasedAnn.Type Prim.PrimTy,
    term :: term
  }
  deriving (Show)

-- TODO ∷ replace more data types with SEXPs for easier
-- processing... Might end up with having more than 1 of these which
-- transformation is quite verbose
data TermClosure
  = Var NameSymbol.T
  | -- index into a given closure
    ArrayIndex IndexInto
  | Prim Prim.RawPrimVal
  | -- Removing captures from LamM
    LamM
      { arguments :: [NameSymbol.T],
        body :: Annotated TermClosure
      }
  | -- Addition to Core.
    Closure
      { capture :: [Capture],
        argumentOffsets :: [ArraySlot],
        body :: Annotated TermClosure
      }
  | PairM
      (Annotated TermClosure)
      (Annotated TermClosure)
  | CatProductIntroM
      (Annotated TermClosure)
      (Annotated TermClosure)
  | CatProductElimLeftM
      (Annotated TermClosure)
      (Annotated TermClosure)
  | CatProductElimRightM
      (Annotated TermClosure)
      (Annotated TermClosure)
  | CatCoproductIntroLeftM (Annotated TermClosure)
  | CatCoproductIntroRightM (Annotated TermClosure)
  | CatCoproductElimM
      (Annotated TermClosure)
      (Annotated TermClosure)
      (Annotated TermClosure)
      (Annotated TermClosure)
      (Annotated TermClosure)
  | UnitM
  | AppM
      (Annotated TermClosure)
      [Annotated TermClosure]
  deriving (Show)
