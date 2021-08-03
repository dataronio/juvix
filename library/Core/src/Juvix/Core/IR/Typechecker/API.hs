module Juvix.Core.IR.Typechecker.API
  (Typechecker,
  checkType,
  Juvix.Core.IR.Typechecker.API.reduce,
  normalize)
  where

import Juvix.Library
import Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR

data Error = OopsTypeError

data Ctxt = Context
  -- must check which values we usually use here
  -- Env.TypeCheck IR.T primTy primVal m [Core.RawGlobal extT primTy primVal]
  deriving (Generic, Show)

data TypecheckerI = TypecheckerI
  { context :: Ctxt
  }
  deriving (Generic, Show)

type TypecheckerA =
  ExceptT Error (State TypecheckerI)

newtype Typechecker a = Typechecker {_run :: TypecheckerA a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "context" Ctxt,
      HasSource "context" Ctxt
    )
    via ReaderField "context" TypecheckerA
  deriving
    (HasThrow "error" Error)
    via MonadError TypecheckerA

checkType :: Typechecker a -> Typechecker a
checkType = undefined

reduce :: Typechecker a -> Typechecker a
reduce = undefined

normalize :: Typechecker a -> Typechecker a
normalize = undefined
