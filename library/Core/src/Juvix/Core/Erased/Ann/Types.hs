module Juvix.Core.Erased.Ann.Types where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Juvix.Core.Application (IsParamVar (..))
import Juvix.Core.Base.Types (Universe)
import Juvix.Core.Parameterisation (KindedType', TypedPrim')
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

------------------------------------------------------------------------------

data T

instance IsParamVar T where
  type ParamVar T = NameSymbol.T
  freeVar _ = Just
  boundVar _ _ = Nothing

type Prim ty val = TypedPrim' T ty val

type PrimTy ty = KindedType' T ty

type TypeT ty = Type (PrimTy ty)

type TermT ty val = Term (PrimTy ty) (Prim ty val)

type AnnTermT ty val = AnnTerm (PrimTy ty) (Prim ty val)

data Term primTy primVal
  = Var NameSymbol.T
  | Prim primVal
  | LamM
      { capture :: [NameSymbol.T], -- Capture
        arguments :: [NameSymbol.T], -- Arguments
        -- the Term in AnnTerm is not lam!
        body :: AnnTerm primTy primVal
      }
  | PairM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | CatProductIntroM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | CatProductElimLeftM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | CatProductElimRightM (AnnTerm primTy primVal) (AnnTerm primTy primVal)
  | CatCoproductIntroLeftM (AnnTerm primTy primVal)
  | CatCoproductIntroRightM (AnnTerm primTy primVal)
  | CatCoproductElimM
      (AnnTerm primTy primVal)
      (AnnTerm primTy primVal)
      (AnnTerm primTy primVal)
      (AnnTerm primTy primVal)
      (AnnTerm primTy primVal)
  | UnitM
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Read, Eq, Generic)

data Type primTy
  = SymT NameSymbol.T
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  | Sig Usage.T (Type primTy) (Type primTy)
  | CatProduct (Type primTy) (Type primTy)
  | CatCoproduct (Type primTy) (Type primTy)
  | UnitTy
  deriving (Show, Read, Eq, Generic)

data AnnTerm primTy primVal = Ann
  { usage :: Usage.T,
    type' :: Type primTy,
    term :: Term primTy primVal
  }
  deriving (Show, Read, Eq, Generic)

pattern AnnAny :: Type primTy -> Term primTy primVal -> AnnTerm primTy primVal
pattern AnnAny {typeA, termA} =
  Ann {usage = Usage.SAny, type' = typeA, term = termA}

-- TODO ∷ make usageFromType Fold!
usageFromType :: Type primTy -> [Usage.T]
usageFromType (Pi usage _ xs) = usage : usageFromType xs
usageFromType (Sig usage _ xs) = usage : usageFromType xs
usageFromType _ = []

piToReturnType :: Type primTy -> Type primTy
piToReturnType (Pi _ _ rest) = piToReturnType rest
piToReturnType last = last

piToList :: Type primTy -> [(Usage.T, Type primTy)]
piToList (Pi usage aType rest) = (usage, aType) : piToList rest
piToList _ = []

piToListTy :: Type primTy -> [Type primTy]
piToListTy (Pi _usage ty xs) = ty : piToListTy xs
piToListTy _ = []

------------------------------------------------------------------------------
-- JSON Instances
------------------------------------------------------------------------------

instance (A.ToJSON ty, A.ToJSON val) => A.ToJSON (Term ty val) where
  toJSON =
    A.genericToJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

instance (A.FromJSON ty, A.FromJSON val) => A.FromJSON (Term ty val) where
  parseJSON =
    A.genericParseJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

instance (A.ToJSON ty) => A.ToJSON (Type ty) where
  toJSON =
    A.genericToJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

instance (A.FromJSON ty) => A.FromJSON (Type ty) where
  parseJSON =
    A.genericParseJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

instance (A.ToJSON ty, A.ToJSON val) => A.ToJSON (AnnTerm ty val) where
  toJSON =
    A.genericToJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )

instance (A.FromJSON ty, A.FromJSON val) => A.FromJSON (AnnTerm ty val) where
  parseJSON =
    A.genericParseJSON
      ( A.defaultOptions
          { A.sumEncoding = A.ObjectWithSingleField
          }
      )
