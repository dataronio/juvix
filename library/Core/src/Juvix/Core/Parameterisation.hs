{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Juvix parameterises the type theory & core language over a set of
-- primitive data types and primitive values, which can include native data
-- types such as strings, integers, or sets, and native functions such as
-- addition, subtraction, string concatenation, set membership, etc. The
-- language & typechecker can then be instantiated over a particular backend
-- which provides concrete sets of primitives and a primitive type-checking
-- relation.
module Juvix.Core.Parameterisation
  ( Parameterisation (..),
    Builtins,
    PrimType (..),
    TypedPrim,
    TypedPrim',
    PrimTake,
    PrimArg',
    PrimArg,
    ApplyError,
    ApplyError' (..),
    CanApply (..),
    CanPrimApply (..),
    apply1,
    apply1Maybe,
    applyMaybe,
    mapApplyErr,
    check3Equal,
    check2Equal,
    checkFirst2AndLast,
    splitReturn,
  )
where

import qualified Data.Aeson as A
import qualified Juvix.Core.Application as App
import Juvix.Core.Base.Types (BoundVar, GlobalName)
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import Juvix.Library.HashMap (HashMap)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage
import qualified Data.List.NonEmpty as NonEmpty

-- | @[A, B, ..., Z]@ represents the type
-- @π A -> ρ B -> ... -> Z@ for any usages @π@, @ρ@
newtype PrimType primTy = PrimType {getPrimType :: NonEmpty primTy}
  deriving stock (Eq, Ord, Show, Read, Generic, Traversable)
  deriving newtype (Functor, Foldable)

instance (A.ToJSON primTy) => A.ToJSON (PrimType primTy) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON primTy) => A.FromJSON (PrimType primTy) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- | A HashMap of builtins with their name.
type Builtins p = HashMap NameSymbol.T p

-- | Abstraction over backend specifics.
data Parameterisation primTy primVal = Parameterisation
  { -- | Check if a value is of a given type.
    hasType :: primVal -> PrimType primTy -> Bool,
    -- | Set of builtin types.
    builtinTypes :: Builtins primTy,
    -- | Set of builtin values.
    builtinValues :: Builtins primVal,
    -- | Build a value from a string.
    stringVal :: Text -> Maybe primVal,
    -- | Build a value from an integer.
    intVal :: Integer -> Maybe primVal,
    -- | Build a value from a float.
    floatVal :: Double -> Maybe primVal
  }
  deriving (Generic)

-- | Datatype representing different kinds of errors.
data ApplyError' e a
  = ExtraArguments a (NonEmpty (Arg a))
  | InvalidArguments a (NonEmpty (Arg a))
  | Extra e

deriving instance (Eq e, Eq a, Eq (Arg a)) => Eq (ApplyError' e a)

deriving instance (Show e, Show a, Show (Arg a)) => Show (ApplyError' e a)

-- | Just like 'ApplyError'', but with a default for the extra argument.
type ApplyError a = ApplyError' (ApplyErrorExtra a) a

type instance PP.Ann (ApplyError' _ _) = HR.PPAnn

instance
  ( PP.PrettyText e,
    HR.ToPPAnn (PP.Ann e),
    PP.PrettySyntax (Arg a),
    HR.ToPPAnn (PP.Ann (Arg a)),
    PP.PrettySyntax a,
    HR.ToPPAnn (PP.Ann a)
  ) =>
  PP.PrettyText (ApplyError' e a)
  where
  prettyT = \case
    ExtraArguments f xs ->
      PP.sepIndent'
        [ (False, "Function"),
          (True, pretty0 f),
          (False, "applied to extra arguments"),
          (True, PP.sep $ PP.punctuate "," $ fmap pretty0 xs)
        ]
    InvalidArguments f xs ->
      PP.sepIndent'
        [ (False, "Function"),
          (True, pretty0 f),
          (False, "applied to invalid arguments"),
          (True, PP.sep $ PP.punctuate "," $ fmap pretty0 xs)
        ]
    Extra e -> HR.toPPAnn <$> PP.prettyT e
    where
      pretty0 = fmap HR.toPPAnn . PP.pretty0

-- | Class that implements application for its argument.
class CanApply a where
  -- | Type to represent extra errors that might occur during application, if
  -- any. Defaults to 'Void'.
  type ApplyErrorExtra a
  type ApplyErrorExtra a = Void

  -- | Type to use for arguments. Defaults to @a@.
  type Arg a
  type Arg a = a

  -- | Lift a value into an argument.
  pureArg :: a -> Maybe (Arg a)
  default pureArg :: (Arg a ~ a) => a -> Maybe (Arg a)
  pureArg = Just

  -- | Create a reference to a free variable.
  freeArg :: Proxy a -> GlobalName -> Maybe (Arg a)
  -- TODO: This may be removed
  freeArg _ _ = Nothing

  -- | Create a reference to a bound variable.
  boundArg :: Proxy a -> BoundVar -> Maybe (Arg a)
  boundArg _ _ = Nothing

  -- | Arity of the function.
  arity :: a -> Natural

  -- | Apply using a non-empty list of arguments
  apply :: a -> NonEmpty (Arg a) -> Either (ApplyError a) a


class CanPrimApply ty tm | tm -> ty where
  -- | Type to represent extra errors that might occur during interpretation of
  -- primitives, if any. Defaults to 'Void'.
  type PrimApplyError tm
  type PrimApplyError tm = Void

  -- | Apply a primitive function to some arguments.
  -- It can be assumed that the argument list is of the correct length
  -- and types.
  primApply :: PrimTake ty tm -> NonEmpty (PrimTake ty tm)
            -> Either (PrimApplyError tm) (PrimType ty, tm)

  -- | Arity of a primitive.
  primArity :: tm -> Natural


instance (App.IsParamVar ext, CanPrimApply ty tm) =>
  CanApply (TypedPrim' ext ty tm)
 where
  type ApplyErrorExtra (TypedPrim' ext ty tm) = PrimApplyError tm
  type Arg             (TypedPrim' ext ty tm) = App.Arg' ext (PrimType ty) tm

  freeArg  _ x = App.VarArg <$> App.freeVar  (Proxy @ext) x
  boundArg _ i = App.VarArg <$> App.boundVar (Proxy @ext) i

  pureArg = Just . App.TermArg

  arity (splitReturn -> (_, _, ar)) = ar

  apply ret@(splitReturn -> (fun, args1, ar)) args2 =
    let argLen = fromIntegral $ length args2
        args   = foldr NonEmpty.cons args2 args1
    in
    case argLen `compare` ar of
      LT -> Right $ App.Cont {fun, args = toList args, numLeft = ar - argLen}
      EQ
        | Just args <- traverse App.argToTake args ->
            primApply fun args |> bimap Extra (uncurry App.Return)
        | otherwise -> Right $ App.Cont {fun, args = toList args, numLeft = 0}
      GT -> Left $ ExtraArguments ret args2


-- | Split a 'Return' into its head and its arguments, and its remaining arity
-- (i.e., the arity of the head minus the length of the arguments).
--
-- (This function is in this module instead of "Juvix.Core.Application" because
-- it needs a 'CanPrimApply' constraint.)
splitReturn :: CanPrimApply ty term
            => TypedPrim' ext ty term
            -> (PrimTake ty term, [PrimArg' ext ty term], Natural)
splitReturn (App.Cont   {fun, args, numLeft}) = (fun, args, numLeft)
splitReturn (App.Return {retType, retTerm})   = (fun, [], ar) where
  fun = App.Take {type' = retType, term = retTerm, usage = Usage.SAny}
  ar  = primArity retTerm


-- | Apply a function 'wrap' over the result of an application.
mapApplyErr ::
  ( ApplyErrorExtra a ~ ApplyErrorExtra b,
    Arg a ~ a,
    Arg b ~ b
  ) =>
  (a -> b) ->
  Either (ApplyError a) a ->
  Either (ApplyError b) b
mapApplyErr wrap = bimap wrap' wrap
  where
    wrap' (ExtraArguments f xs) =
      ExtraArguments (wrap f) (map wrap xs)
    wrap' (InvalidArguments f xs) =
      InvalidArguments (wrap f) (map wrap xs)
    wrap' (Extra e) = Extra e

-- | Version of 'apply' returning a Maybe instead of an Either.
applyMaybe :: CanApply a => a -> NonEmpty (Arg a) -> Maybe a
applyMaybe f xs = either (const Nothing) Just $ apply f xs

-- | Version of 'apply' that requires at least one argument.
apply1 :: CanApply a => a -> Arg a -> Either (ApplyError a) a
apply1 f x = apply f (x :| [])

-- | Version of 'apply1' that requires at returns a Maybe.
apply1Maybe :: CanApply a => a -> Arg a -> Maybe a
apply1Maybe f x = applyMaybe f (x :| [])

-- | A typed primitive which may be partially applies and, if so, may contain
-- variables inside its arguments. The @ext@ is passed to 'App.Arg'' to specify
-- what is allowed as a variable.
type TypedPrim' ext ty val = App.Return' ext (PrimType ty) val

-- | See 'TypedPrim''.
type TypedPrim ty val = TypedPrim' IR.T ty val

-- | A 'App.Take', with the type in the right shape for primitives that can be
-- functions.
type PrimTake ty val = App.Take (PrimType ty) val

-- | A 'App.Arg'', with the type in the right shape for primitives that can be
-- functions.
type PrimArg' ext ty val = App.Arg' ext (PrimType ty) val

type PrimArg ty val = PrimArg' IR.T ty val

-- | Pretty-printing highlight annotations for primitives.
data PPAnn' primTy
  = PAArrow
  | PAPunct
  | PATy (PP.Ann primTy)

type PPAnn primTy = Last (PPAnn' primTy)

type instance PP.Ann (PrimType primTy) = PPAnn primTy

instance PP.PrettySyntax primTy => PP.PrettySyntax (PrimType primTy) where
  pretty' tys =
    PP.parensP' PAPunct PP.Outer $
      PP.sepA (PP.punctuateA arr (map pretty1 tys))
    where
      arr = pure $ PP.annotate' PAArrow " →"
      pretty1 =
        fmap (fmap $ Last . Just . PATy)
          . PP.withPrec (PP.Infix 0)
          . PP.pretty'

instance HR.ToPPAnn (PP.Ann ty) => HR.ToPPAnn (PPAnn ty) where
  toPPAnn a =
    a >>= \case
      PAArrow -> pure HR.ATyCon
      PAPunct -> pure HR.APunct
      PATy a' -> HR.toPPAnn a'

check3Equal :: Eq a => NonEmpty a -> Bool
check3Equal (x :| [y, z])
  | x == y && x == z = True
  | otherwise = False
check3Equal (_ :| _) = False

check2Equal :: Eq a => NonEmpty a -> Bool
check2Equal (x :| [y])
  | x == y = True
  | otherwise = False
check2Equal (_ :| _) = False

checkFirst2AndLast :: Eq t => NonEmpty t -> (t -> Bool) -> Bool
checkFirst2AndLast (x :| [y, last]) check
  | check2Equal (x :| [y]) && check last = True
  | otherwise = False
checkFirst2AndLast (_ :| _) _ = False
