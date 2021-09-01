{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Base.Types.Base where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Data.Kind (Constraint)
import Extensible (Config (..), NameAffix (..), defaultConfig, extensibleWith)
import Juvix.Library hiding (Pos, datatypeName)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage

------------------------------------------------------------------------------

type Universe = Natural

type GlobalName = NameSymbol.T

type PatternVar = Int

-- | set of pattern variables
type PatternSet = IntSet

-- | map from pattern variables to e.g. their types
type PatternMap = IntMap

type BoundVar = Natural

data Name
  = -- | Global variables are represented by name thus type string
    Global GlobalName
  | -- | Pattern variable, unique within a scope
    Pattern PatternVar
  deriving (Show, Eq, Generic, Data, NFData)

instance A.ToJSON Name where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Name where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- TODO: maybe global functions can have any usage? (for private defs)
data GlobalUsage = GZero | GOmega
  deriving (Show, Eq, Generic, Data, Bounded, Enum, NFData)

instance A.ToJSON GlobalUsage where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON GlobalUsage where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

------------------------------------------------------------------------------

extensibleWith
  defaultConfig
    { datatypeName = NameAffix "" "",
      constructorName = NameAffix "" ""
    }
  [d|
    data Term primTy primVal
      = -- | (sort i) i th ordering of (closed) universe.
        Star Universe
      | -- | PrimTy primitive type
        PrimTy primTy
      | -- | primitive constant
        Prim primVal
      | -- | formation rule of the dependent function type PI.
        -- the Usage(π) tracks how many times x is used.
        Pi Usage (Term primTy primVal) (Term primTy primVal)
      | -- | LAM Introduction rule of PI.
        -- The abstracted variables usage is tracked with the Usage(π).
        Lam (Term primTy primVal)
      | -- | Dependent pair (Σ) type, with each half having its own usage
        Sig Usage (Term primTy primVal) (Term primTy primVal)
      | -- | Pair value
        Pair (Term primTy primVal) (Term primTy primVal)
      | -- | Let binder.
        -- the local definition is bound to de Bruijn index 0.
        Let Usage (Elim primTy primVal) (Term primTy primVal)
      | -- | Unit type.
        UnitTy
      | -- | Unit Value
        Unit
      | -- | CONV conversion rule. TODO make sure 0Γ ⊢ S≡T
        -- Elim is the constructor that embeds Elim to Term
        Elim (Elim primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- inferable terms
    data Elim primTy primVal
      = -- | Bound variables, in de Bruijn indices
        Bound BoundVar
      | -- | Free variables of type name (see above)
        Free Name
      | -- | elimination rule of PI (APP).
        App (Elim primTy primVal) (Term primTy primVal)
      | -- | Annotation with usage.
        Ann Usage (Term primTy primVal) (Term primTy primVal) Universe
      deriving (Eq, Show, Generic, Data, NFData)

    -- Values/types
    data Value primTy primVal
      = VStar Universe
      | VPrimTy primTy
      | VPi Usage (Value primTy primVal) (Value primTy primVal)
      | VLam (Value primTy primVal)
      | VSig Usage (Value primTy primVal) (Value primTy primVal)
      | VPair (Value primTy primVal) (Value primTy primVal)
      | VUnitTy
      | VUnit
      | VNeutral (Neutral primTy primVal)
      | VPrim primVal
      deriving (Eq, Show, Generic, Data, NFData)

    -- A neutral term is either a variable or an application of a neutral term
    -- to a value
    data Neutral primTy primVal
      = NBound BoundVar
      | NFree Name
      | NApp (Neutral primTy primVal) (Value primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- TODO absurd pattern
    data Pattern primTy primVal
      = PCon GlobalName [Pattern primTy primVal]
      | PPair (Pattern primTy primVal) (Pattern primTy primVal)
      | PUnit
      | PVar PatternVar
      | PDot (Term primTy primVal)
      | PPrim primVal
      deriving (Show, Eq, Generic, Data, NFData)
    |]

instance (A.ToJSON primTy, A.ToJSON primVal, CoreAll A.ToJSON ext primTy primVal) => A.ToJSON (Term ext primTy primVal) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON primTy, A.FromJSON primVal, CoreAll A.FromJSON ext primTy primVal) => A.FromJSON (Term ext primTy primVal) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.ToJSON primTy, A.ToJSON primVal, CoreAll A.ToJSON ext primTy primVal) => A.ToJSON (Elim ext primTy primVal) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON primTy, A.FromJSON primVal, CoreAll A.FromJSON ext primTy primVal) => A.FromJSON (Elim ext primTy primVal) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.ToJSON primTy, A.ToJSON primVal, ValueAll A.ToJSON ext primTy primVal, NeutralAll A.ToJSON ext primTy primVal) => A.ToJSON (Value ext primTy primVal) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON primTy, A.FromJSON primVal, ValueAll A.FromJSON ext primTy primVal, NeutralAll A.FromJSON ext primTy primVal) => A.FromJSON (Value ext primTy primVal) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.ToJSON primTy, A.ToJSON primVal, ValueAll A.ToJSON ext primTy primVal, NeutralAll A.ToJSON ext primTy primVal) => A.ToJSON (Neutral ext primTy primVal) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON primTy, A.FromJSON primVal, ValueAll A.FromJSON ext primTy primVal, NeutralAll A.FromJSON ext primTy primVal) => A.FromJSON (Neutral ext primTy primVal) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.ToJSON primTy, A.ToJSON primVal, CoreAll A.ToJSON ext primTy primVal) => A.ToJSON (Pattern ext primTy primVal) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON primTy, A.FromJSON primVal, CoreAll A.FromJSON ext primTy primVal) => A.FromJSON (Pattern ext primTy primVal) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

type CoreAll (c :: Type -> Constraint) ext primTy primVal =
  ( TermAll c ext primTy primVal,
    ElimAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

type CoreShow ext primTy primVal = CoreAll Show ext primTy primVal

type CoreEq ext primTy primVal = CoreAll Eq ext primTy primVal

type QuoteContext ext primTy primVal =
  ( XVStar ext primTy primVal ~ XStar ext primTy primVal,
    XVPrimTy ext primTy primVal ~ XPrimTy ext primTy primVal,
    XVPi ext primTy primVal ~ XPi ext primTy primVal,
    XVLam ext primTy primVal ~ XLam ext primTy primVal,
    XVSig ext primTy primVal ~ XSig ext primTy primVal,
    XVPair ext primTy primVal ~ XPair ext primTy primVal,
    XVUnitTy ext primTy primVal ~ XUnitTy ext primTy primVal,
    XVUnit ext primTy primVal ~ XUnit ext primTy primVal,
    XVPrim ext primTy primVal ~ XPrim ext primTy primVal,
    XVNeutral ext primTy primVal ~ XElim ext primTy primVal,
    XVPrimTy ext primTy primVal ~ XPrimTy ext primTy primVal,
    ValueX ext primTy primVal ~ TermX ext primTy primVal,
    XNBound ext primTy primVal ~ XBound ext primTy primVal,
    XNFree ext primTy primVal ~ XFree ext primTy primVal,
    XNApp ext primTy primVal ~ XApp ext primTy primVal,
    NeutralX ext primTy primVal ~ ElimX ext primTy primVal
  )

-- Quotation: takes a value back to a term
quote :: QuoteContext ext primTy primVal => Value ext primTy primVal -> Term ext primTy primVal
quote (VStar nat ext) = Star nat ext
quote (VPrimTy p ext) = PrimTy p ext
quote (VPi π s t ext) = Pi π (quote s) (quote t) ext
quote (VLam s ext) = Lam (quote s) ext
quote (VSig π s t ext) = Sig π (quote s) (quote t) ext
quote (VPair s t ext) = Pair (quote s) (quote t) ext
quote (VUnitTy ext) = UnitTy ext
quote (VUnit ext) = Unit ext
quote (VPrim pri ext) = Prim pri ext
quote (VNeutral n ext) = Elim (neutralQuote n) ext
quote (ValueX ext) = TermX ext

neutralQuote :: QuoteContext ext primTy primVal => Neutral ext primTy primVal -> Elim ext primTy primVal
neutralQuote (NBound x ext) = Bound x ext
neutralQuote (NFree x ext) = Free x ext
neutralQuote (NApp n v ext) = App (neutralQuote n) (quote v) ext
neutralQuote (NeutralX ext) = ElimX ext

-- | 'VFree' creates the value corresponding to a free variable
pattern VFree ::
  ( XNFree ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  Name ->
  Value ext primTy primVal
pattern VFree n = VNeutral (NFree n ()) ()

-- | 'VBound' creates the value corresponding to a bound variable
pattern VBound ::
  ( XNBound ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  BoundVar ->
  Value ext primTy primVal
pattern VBound n = VNeutral (NBound n ()) ()
