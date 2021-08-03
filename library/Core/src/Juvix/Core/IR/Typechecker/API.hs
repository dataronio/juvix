module Juvix.Core.IR.Typechecker.API
  (Typechecker,
  checkType,
  reduce,
  normalize)
  where

import Juvix.Library
import Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR

data Error = OopsTypeError

type Ctxt = forall primTy primVal m extT.
  (Env.CanTC' extT primTy primVal m,
   Eval.CanEval extT IR.T primTy primVal
  ) =>
  Env.TypeCheck IR.T primTy primVal m [Core.RawGlobal extT primTy primVal]

data TypecheckerI = TypecheckerI
  { context :: Ctxt
  }
  deriving (Generic, Show)

type TypecheckerA a =
  ExceptT Error (State TypecheckerI)

newtype Typechecker a = Typechecker {_run :: TypecheckerA}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "context" a,
      HasSource "context" a
    )
    via ReaderField "context" TypecheckerA
  deriving
    (HasThrow "error" Error)
    via MonadError TypecheckerA

checkType :: Typechecker -> Typechecker
checkType = undefined

reduce :: Typechecker -> Typechecker
reduce = undefined

normalize :: Typechecker -> Typechecker
normalize = undefined
