{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Types
  ( module Juvix.Core.Types,
    module Juvix.Core.Parameterisation,
  )
where

-- import qualified Juvix.Core.EAC.Types as EAC
import qualified Juvix.Core.Erased as EC
import qualified Juvix.Core.Erased.Algorithm.Types as Erasure
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.HR.Types as HR
import qualified Juvix.Core.IR.Typechecker as Typed
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Parameterisation
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP

-- | Errors used throughout the pipeline.
data PipelineError primTy primVal compErr
  = InternalInconsistencyError Text
  | -- | Currently, we exit with this when a user program doesn't typecheck.
    --   I expect the long-term plan is to convert it to a human-readable
    --   error.
    TypecheckerError (Typed.TypecheckError primTy primVal)
  | -- EACError (EAC.Errors primTy primVal)
    ErasureError (Erasure.Error (KindedType primTy) (TypedPrim primTy primVal))
  | PrimError compErr
  deriving (Generic)

deriving instance
  ( Show primTy,
    Show primVal,
    Show compErr,
    Show (PrimApplyError primTy),
    Show (PrimApplyError primVal)
  ) =>
  Show (PipelineError primTy primVal compErr)

type instance PP.Ann (PipelineError _ _ _) = HR.PPAnn

instance
  ( HR.PrettyText' compErr,
    HR.PrettyText' (PrimApplyError primTy),
    HR.PrettyText' (ApplyErrorExtra primTy),
    HR.PrettyText' (ApplyErrorExtra (TypedPrim primTy primVal)),
    HR.PrimPretty primTy (TypedPrim primTy primVal),
    HR.PrimPretty (Arg primTy) (Arg (TypedPrim primTy primVal)),
    HR.PrimPretty1 primVal
  ) =>
  PP.PrettyText (PipelineError primTy primVal compErr)
  where
  prettyT = \case
    InternalInconsistencyError txt -> PP.text txt
    TypecheckerError err -> PP.prettyT err
    ErasureError err -> PP.prettyT err
    PrimError err -> HR.toPPAnn <$> PP.prettyT err

data PipelineLog primTy primVal
  = LogHRtoIR (HR.Term primTy primVal) (IR.Term primTy primVal)
  | LogRanZ3 Double
  deriving (Show, Generic)

-- | Map from a term to a formula of Elementary Affine Logic (EAL).
-- compErr serves to resolve the compilation error type
-- needed to promote a backend specific compilation error
data TermAssignment primTy primVal compErr = Assignment
  { term :: EC.Term primVal,
    assignment :: EC.TypeAssignment primTy
  }
  deriving (Show, Generic)

-- | A 'TermAssignment' additionally annotated with a type.
data AssignWithType primTy primVal compErr = WithType
  { termAssign :: TermAssignment primTy primVal compErr,
    type' :: EC.Type primTy
  }
  deriving (Show, Generic)
