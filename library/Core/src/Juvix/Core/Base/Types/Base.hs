{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Base.Types.Base
  ( module Juvix.Core.Base.Types.Base,
    Universe (.., U),
  )
where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Data.Kind (Constraint)
import Extensible (Config (..), NameAffix (..), defaultConfig, extensibleWith)
import Juvix.Library hiding (Pos, datatypeName)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage

------------------------------------------------------------------------------

-- | a concrete universe level is just a natural number.
newtype ConcUniverse = CU Natural
  deriving (Show, Read, Eq, Ord, Generic, Data)
  deriving newtype (NFData)

instance A.ToJSON ConcUniverse where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ConcUniverse where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- | a general universe is either a concrete universe, or a marker saying
-- that we don't care about the universe a type lives in. 'UAny' never shows up
-- in source programs, but is needed in e.g. the typechecker when ensuring that
-- the type in an annotation expression is /a/ type, of any universe.
data Universe
  = U' ConcUniverse
  | UAny
  deriving (Show, Read, Eq, Ord, Generic, Data, NFData)

pattern U :: Natural -> Universe
pattern U i = U' (CU i)

{-# COMPLETE U, UAny #-}

instance A.ToJSON Universe where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Universe where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

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
data GlobalUsage = GZero | GSAny
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
      | -- | Category-theoretical product type.
        -- Distinct from Pair because in its dependent form, the right-hand
        -- side will inherently be a Pi type whose domain is the type of the
        -- left-hand side, and whose codomain is type-valued.
        CatProduct (Term primTy primVal) (Term primTy primVal)
      | -- | Category-theoretical coproduct type.  In its dependent form, it will have
        -- a type-valued domain parameter, and the left-hand side and right-hand
        -- side will both be Pi types with that same domain, and with
        -- type-valued codomains.
        CatCoproduct (Term primTy primVal) (Term primTy primVal)
      | -- | Higher-order introduction rule for category-theoretical product.
        CatProductIntro (Term primTy primVal) (Term primTy primVal)
      | -- | Higher-order left projection for category-theoretical product.
        CatProductElimLeft (Term primTy primVal) (Term primTy primVal)
      | -- | Higher-order right projection for category-theoretical product.
        CatProductElimRight (Term primTy primVal) (Term primTy primVal)
      | -- | Higher-order left injection for category-theoretical coproduct.
        CatCoproductIntroLeft (Term primTy primVal)
      | -- | Higher-order right injection for category-theoretical coproduct.
        CatCoproductIntroRight (Term primTy primVal)
      | -- | Higher-order elimination category-theoretical coproduct (in
        -- effect, a case statement).
        CatCoproductElim (Term primTy primVal) (Term primTy primVal) (Term primTy primVal) (Term primTy primVal) (Term primTy primVal)
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
      | -- | Function application.
        App (Elim primTy primVal) (Term primTy primVal)
      | -- | Type annotation.
        Ann (Term primTy primVal) (Term primTy primVal)
      deriving (Eq, Show, Generic, Data, NFData)

    -- Values/types
    data Value primTy primVal
      = VStar Universe
      | VPrimTy primTy
      | VPi Usage (Value primTy primVal) (Value primTy primVal)
      | VLam (Value primTy primVal)
      | VSig Usage (Value primTy primVal) (Value primTy primVal)
      | VPair (Value primTy primVal) (Value primTy primVal)
      | VCatProduct (Value primTy primVal) (Value primTy primVal)
      | VCatCoproduct (Value primTy primVal) (Value primTy primVal)
      | VCatProductIntro (Value primTy primVal) (Value primTy primVal)
      | VCatProductElimLeft (Value primTy primVal) (Value primTy primVal)
      | VCatProductElimRight (Value primTy primVal) (Value primTy primVal)
      | VCatCoproductIntroLeft (Value primTy primVal)
      | VCatCoproductIntroRight (Value primTy primVal)
      | VCatCoproductElim (Value primTy primVal) (Value primTy primVal) (Value primTy primVal) (Value primTy primVal) (Value primTy primVal)
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
    XVCatProduct ext primTy primVal ~ XCatProduct ext primTy primVal,
    XVCatCoproduct ext primTy primVal ~ XCatCoproduct ext primTy primVal,
    XVCatProductIntro ext primTy primVal ~ XCatProductIntro ext primTy primVal,
    XVCatProductElimLeft ext primTy primVal ~ XCatProductElimLeft ext primTy primVal,
    XVCatProductElimRight ext primTy primVal ~ XCatProductElimRight ext primTy primVal,
    XVCatCoproductIntroLeft ext primTy primVal ~ XCatCoproductIntroLeft ext primTy primVal,
    XVCatCoproductIntroRight ext primTy primVal ~ XCatCoproductIntroRight ext primTy primVal,
    XVCatCoproductElim ext primTy primVal ~ XCatCoproductElim ext primTy primVal,
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
quote (VCatProduct s t ext) = CatProduct (quote s) (quote t) ext
quote (VCatCoproduct s t ext) = CatCoproduct (quote s) (quote t) ext
quote (VCatProductIntro s t ext) = CatProductIntro (quote s) (quote t) ext
quote (VCatProductElimLeft a s ext) = CatProductElimLeft (quote a) (quote s) ext
quote (VCatProductElimRight a s ext) = CatProductElimRight (quote a) (quote s) ext
quote (VCatCoproductIntroLeft s ext) = CatCoproductIntroLeft (quote s) ext
quote (VCatCoproductIntroRight s ext) = CatCoproductIntroRight (quote s) ext
quote (VCatCoproductElim a b cp s t ext) = CatCoproductElim (quote a) (quote b) (quote cp) (quote s) (quote t) ext
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
