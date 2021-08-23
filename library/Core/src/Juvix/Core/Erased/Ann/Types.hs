module Juvix.Core.Erased.Ann.Types where

import qualified Data.Aeson as A
import Juvix.Core.Application (IsParamVar (..))
import Juvix.Core.Base.Types (Universe)
import Juvix.Core.Parameterisation (TypedPrim')
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data T

instance IsParamVar T where
  type ParamVar T = NameSymbol.T
  freeVar _ = Just
  boundVar _ _ = Nothing

type TypedPrim ty val = TypedPrim' T ty val

type TermT ty val = Term ty (TypedPrim ty val)

type AnnTermT ty val = AnnTerm ty (TypedPrim ty val)

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
  | UnitM
  | AppM (AnnTerm primTy primVal) [AnnTerm primTy primVal]
  deriving (Show, Read, Eq, Generic)

instance (A.ToJSON ty, A.ToJSON val) => A.ToJSON (Term ty val) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON ty, A.FromJSON val) => A.FromJSON (Term ty val) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Type primTy
  = SymT NameSymbol.T
  | Star Universe
  | PrimTy primTy
  | -- TODO: How to deal with dependency?
    Pi Usage.T (Type primTy) (Type primTy)
  | Sig Usage.T (Type primTy) (Type primTy)
  | UnitTy
  deriving (Show, Read, Eq, Generic)

instance (A.ToJSON ty) => A.ToJSON (Type ty) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON ty) => A.FromJSON (Type ty) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data AnnTerm primTy primVal = Ann
  { usage :: Usage.T,
    type' :: Type primTy,
    term :: Term primTy primVal
  }
  deriving (Show, Read, Eq, Generic)

instance (A.ToJSON ty, A.ToJSON val) => A.ToJSON (AnnTerm ty val) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON ty, A.FromJSON val) => A.FromJSON (AnnTerm ty val) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})
