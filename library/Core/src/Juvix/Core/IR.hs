module Juvix.Core.IR
  ( module Juvix.Core.IR,
    module IR,
  )
where

import Juvix.Core.IR.CheckTerm as IR
import Juvix.Core.IR.Evaluator as IR
import qualified Juvix.Core.IR.Typechecker as Typed
import Juvix.Core.IR.Types as IR
import Juvix.Library

type TermT primTy primVal = Typed.Term primTy primVal

type ElimT primTy primVal = Typed.Elim primTy primVal

execTC ::
  Typed.GlobalsT IR.T IR.T primTy primVal ->
  Typed.EnvTypecheck primTy primVal a ->
  (Either (Typed.TypecheckError primTy primVal) a, Typed.EnvCtx primTy primVal)
execTC = Typed.exec
