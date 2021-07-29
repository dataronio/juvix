module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term (..),
    Type' (..),
    TypeAssignment',
  )
where

import Juvix.Core.Erased.Base.Types
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Library

data T
  deriving (Show, Read, Data)

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

type TermT primTy primVal = Term (Typed.Prim primTy primVal)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

type TypeAssignment primTy = TypeAssignment' T primTy
