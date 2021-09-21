{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Types
  ( module Juvix.Core.IR.Typechecker.Types,

    -- * Constructors & fields for 'Return'
    pattern App.Cont,
    App.fun,
    App.args,
    App.numLeft,
    pattern App.Return,
    App.retType,
    App.retTerm,

    -- * Constructors & fields for 'Take'
    pattern App.Take,
    App.usage,
    App.type',
    App.term,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

data Annotation ext primTy primVal = Annotation
  { annUsage :: Usage.T,
    annType :: Core.Value ext primTy primVal
  }
  deriving (Generic)

deriving instance
  (Eq (Core.Value ext primTy primVal)) =>
  Eq (Annotation ext primTy primVal)

deriving instance
  (Show (Core.Value ext primTy primVal)) =>
  Show (Annotation ext primTy primVal)

data T

data BindAnnotation ext primTy primVal = BindAnnotation
  { baBindAnn, baResAnn :: {-# UNPACK #-} !(Annotation ext primTy primVal)
  }
  deriving (Generic)

deriving instance
  (Eq (Core.Value ext primTy primVal)) =>
  Eq (BindAnnotation ext primTy primVal)

deriving instance
  (Show (Core.Value ext primTy primVal)) =>
  Show (BindAnnotation ext primTy primVal)

Core.extendTerm "Term'" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation IR.T $primTy $primVal|]]
        bindTyped = Just [[t|BindAnnotation IR.T $primTy $primVal|]]
     in Core.defaultExtTerm
          { Core.typeStar = typed,
            Core.typePrimTy = typed,
            Core.typePrim = typed,
            Core.typePi = typed,
            Core.typeSig = typed,
            Core.typePair = typed,
            Core.typeCatProduct = typed,
            Core.typeCatCoproduct = typed,
            Core.typeCatProductIntro = typed,
            Core.typeCatProductElimLeft = typed,
            Core.typeCatProductElimRight = typed,
            Core.typeCatCoproductIntroLeft = typed,
            Core.typeCatCoproductIntroRight = typed,
            Core.typeCatCoproductElim = typed,
            Core.typeUnitTy = typed,
            Core.typeUnit = typed,
            Core.typeLam = bindTyped,
            Core.typeLet = bindTyped,
            Core.typeElim = typed
          }

Core.extendElim "Elim'" [] [t|T|] $
  \primTy primVal ->
    let typed = Just [[t|Annotation IR.T $primTy $primVal|]]
     in Core.defaultExtElim
          { Core.typeBound = typed,
            Core.typeFree = typed,
            Core.typeApp = typed,
            Core.typeAnn = typed
          }

type Term primTy primVal = Term' (PrimTy primTy) (Prim primTy primVal)

type Elim primTy primVal = Elim' (PrimTy primTy) (Prim primTy primVal)

type Prim primTy primVal = P.TypedPrim primTy primVal

type PrimTy primTy = P.KindedType primTy

-- TODO: Remove T
type GlobalT extV extT primTy primVal =
  Core.Global extV extT (PrimTy primTy) (Prim primTy primVal)

type DatatypeT ext primTy primVal =
  Core.Datatype ext (PrimTy primTy) (Prim primTy primVal)

type DataArgT ext primTy primVal =
  Core.DataArg ext (PrimTy primTy) (Prim primTy primVal)

type DataConT ext primTy primVal =
  Core.DataCon ext (PrimTy primTy) (Prim primTy primVal)

type FunctionT extV extT primTy primVal =
  Core.Function extV extT (PrimTy primTy) (Prim primTy primVal)

type FunClauseT extT primTy primVal =
  Core.FunClause extT (PrimTy primTy) (Prim primTy primVal)

type PatternT ext primTy primVal =
  Core.Pattern ext (PrimTy primTy) (Prim primTy primVal)

type AbstractT extV primTy primVal =
  Core.Abstract extV (PrimTy primTy) (Prim primTy primVal)

type GlobalsT extV extT primTy primVal =
  Core.Globals extV extT (PrimTy primTy) (Prim primTy primVal)

type ValueT ext primTy primVal =
  Core.Value ext (PrimTy primTy) (Prim primTy primVal)

type NeutralT ext primTy primVal =
  Core.Neutral ext (PrimTy primTy) (Prim primTy primVal)

type AnnotationT ext primTy primVal =
  Annotation ext (PrimTy primTy) (Prim primTy primVal)

type BindAnnotationT ext primTy primVal =
  BindAnnotation ext (PrimTy primTy) (Prim primTy primVal)

getTermAnn :: Core.Term T primTy primVal -> Annotation IR.T primTy primVal
getTermAnn (Star _ ann) = ann
getTermAnn (PrimTy _ ann) = ann
getTermAnn (Prim _ ann) = ann
getTermAnn (Pi _ _ _ ann) = ann
getTermAnn (Sig _ _ _ ann) = ann
getTermAnn (Pair _ _ ann) = ann
getTermAnn (CatProduct _ _ ann) = ann
getTermAnn (CatCoproduct _ _ ann) = ann
getTermAnn (CatProductIntro _ _ ann) = ann
getTermAnn (CatProductElimLeft _ _ ann) = ann
getTermAnn (CatProductElimRight _ _ ann) = ann
getTermAnn (CatCoproductIntroLeft _ ann) = ann
getTermAnn (CatCoproductIntroRight _ ann) = ann
getTermAnn (CatCoproductElim _ _ _ _ _ ann) = ann
getTermAnn (UnitTy ann) = ann
getTermAnn (Unit ann) = ann
getTermAnn (Lam _ anns) = baResAnn anns
getTermAnn (Let _ _ _ anns) = baResAnn anns
getTermAnn (Elim _ ann) = ann

getElimAnn :: Core.Elim T primTy primVal -> Annotation IR.T primTy primVal
getElimAnn (Bound _ ann) = ann
getElimAnn (Free _ ann) = ann
getElimAnn (App _ _ ann) = ann
getElimAnn (Ann _ _ _ _ ann) = ann
