module Juvix.Core.Typecheck.API.History where

-- enough for now, but a full-blown history effect many be the way to go
-- history exists in a separate file to allow faster amends later
data Log a
  = GlobalSet
  | NewProof
  | NewTerm
  | Typecheck
  | Unification
  | Focus
  | PrimUnification
  | UserOp a

type History a b = (Log a, b)
