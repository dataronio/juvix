{-# LANGUAGE DeriveAnyClass #-}

module Juvix.Backends.Plonk.Types
  ( CompilationError (..),
    AnnTerm,
    Term,
    Type,
    PrimValHR,
    PrimValIR,
    PrimVal',
    ArgHR,
    ArgIR,
    Arg',
    Take,
    ReturnHR,
    ReturnIR,
    Return',
    PrimTy (..),
    PrimVal (..),
    isConst,
    isBinOp,
  )
where

import qualified Data.Aeson as A
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Type)

data PrimVal f
  = PConst f
  | -- UnOps
    PIsZero
  | PNot
  | PShL
  | PShR
  | PRotL
  | PRotR
  | PAssertEq
  | PAssertIt
  | -- BinOps
    PAdd
  | PSub
  | PMul
  | PDiv
  | PExp
  | PMod
  | PAnd
  | POr
  | PXor
  | -- CompOps
    PGt
  | PGte
  | PLt
  | PLte
  | PEq
  deriving (Show, Read, Eq, Generic, Data)

instance A.ToJSON f => A.ToJSON (PrimVal f) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON f => A.FromJSON (PrimVal f) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data PrimTy f
  = PField
  | PInt
  | PBool
  | PApplication (PrimTy f) (NonEmpty (PrimTy f))
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON f => A.ToJSON (PrimTy f) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON f => A.FromJSON (PrimTy f) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

type Return' ext f = App.Return' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ReturnIR f = Return' IR.T f

type ReturnHR f = Return' ErasedAnn.T f

type Take f = App.Take (P.PrimType (PrimTy f)) (PrimVal f)

type Arg' ext f = App.Arg' ext (P.PrimType (PrimTy f)) (PrimVal f)

type ArgIR f = Arg' IR.T f

type ArgHR f = Arg' ErasedAnn.T f

type PrimVal' ext f = Return' ext f

type PrimValIR f = PrimVal' IR.T f

type PrimValHR f = PrimVal' ErasedAnn.T f

type Type f = ErasedAnn.Type (PrimTy f)

type Term f = ErasedAnn.Term (PrimTy f) (PrimVal f)

type AnnTerm f = ErasedAnn.AnnTerm (PrimTy f) (PrimVal f)

newtype CompilationError f
  = NotYetImplemented Text
  deriving (Show, Eq, Generic)

isConst :: PrimVal f -> Bool
isConst (PConst _) = True
isConst _ = False

isBinOp :: PrimVal f -> Bool
isBinOp = \case
  PAdd -> True
  PSub -> True
  PMul -> True
  PDiv -> True
  PExp -> True
  PMod -> True
  PAnd -> True
  POr -> True
  PXor -> True
  PGt -> True
  PGte -> True
  PLt -> True
  PLte -> True
  PEq -> True
