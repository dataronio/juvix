{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Core.HR.Pretty
  ( PPAnn' (..),
    PPAnn,
    ToPPAnn (..),
    Doc,
    PrimPretty1,
    PrimPretty,
    PrettyText,

    -- * extra combinators

    -- ** punctuation with highlighting
    parens,
    parensP,
    angles,
    comma,
    dot,
    colon,
    arrow,
    equals,
    pipe,
    box,

    -- ** formatting constructs
    name,
    pname,
    Bind (..),
    Binder (..),
    WithBinders,
    ppBinders,
    ppUsage,
    ppLams,
    ppOuter,
    ppPairs,
    ppLet,
    ppApps,
    ppStar,
    ppCatCoproduct,
    ppCatCoproductElim,
    ppCatCoproductIntroLeft,
    ppCatCoproductIntroRight,
    ppCatProduct,
    ppCatProductIntro,
    ppCatProductElimLeft,
    ppCatProductElimRight,
  )
where

------------------------------------------------------------------------------

import qualified Juvix.Core.Base as Core
import Juvix.Core.HR.Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage

------------------------------------------------------------------------------

-- | Annotations for syntax highlighting
data PPAnn'
  = -- | builtin type constructors: Π, *, etc
    ATyCon
  | -- | builtin data: λ, (), usages, etc
    AValCon
  | -- | backend primitive type
    APrimTy
  | -- | backend primitive value
    APrimVal
  | -- | backend primitive function
    APrimFun
  | -- | brackets, dots, etc
    APunct
  | -- | names (TODO: distinguish bound & free?)
    AName

type PPAnn = Last PPAnn'

type instance PP.Ann (Term _ _) = PPAnn

type instance PP.Ann (Elim _ _) = PPAnn

type instance PP.Ann (Pattern _ _) = PPAnn

-- | Requirements to be able to print a primitive type or value
type PrimPretty1 p = (PP.PrettySyntax p, ToPPAnn (PP.Ann p))

type PrimPretty ty val = (PrimPretty1 ty, PrimPretty1 val)

type PrettyText a = (PP.PrettyText a, ToPPAnn (PP.Ann a))

-- | Document with syntax highlighting hints
type Doc = PP.Doc PPAnn

-- | Different kind of binders
data Bind = PI | SIG

data Binder tm = Binder
  { bBinder :: Bind,
    bUsage :: Usage.T,
    bName :: NameSymbol.T,
    bType :: tm
  }

type WithBinders tm = ([Binder tm], tm)

------------------------------------------------------------------------------
-- Basic util Doc functions
------------------------------------------------------------------------------

-- | Surround with parens @(…)@.
parens :: Doc -> Doc
parens = PP.parens' APunct

-- | Surround with parens if the current precedence level is greater than the
-- given one, as with 'Text.Show.showsPrec'.
parensP :: PP.PrecReader m => PP.Prec -> m Doc -> m Doc
parensP = PP.parensP' APunct

-- | Surround with angle brackets @‹…›@.
angles :: Doc -> Doc
angles = PP.annotate' APunct "‹" `PP.enclose` PP.annotate' APunct "›"

comma :: Doc
comma = PP.annotate' APunct ","

dot :: Doc
dot = PP.annotate' APunct "."

colon :: Doc
colon = PP.annotate' APunct ":"

arrow :: Doc
arrow = PP.annotate' APunct "→"

equals :: Doc
equals = PP.annotate' APunct "="

pipe :: Doc
pipe = PP.annotate' APunct "|"

-- | ⌷ @APL FUNCTIONAL SYMBOL SQUISH QUAD@, used for printing the unit value
box :: Doc
box = PP.annotate' AValCon "⌷"

-- | Print a @NameSymbol.‌'NameSymbol.T'@ in dotted format and highlighted
-- as a name.
name :: NameSymbol.T -> Doc
name = PP.annotate' AName . PP.string . unintern . NameSymbol.toSymbol

pname :: Applicative f => NameSymbol.T -> f Doc
pname = pure . name

-- TODO: Instead of * better if we use "υ"
universe :: Doc
universe = PP.annotate' ATyCon "*"

-- TODO: Use subindices whenever it's possible, e.g. υ₁ or υ₂.

levelUniverse :: Show a => a -> Doc
levelUniverse i = PP.annotate' ATyCon $ PP.show i

piCon :: Doc
piCon = PP.annotate' ATyCon "Π"

lambda :: Doc
lambda = PP.annotate' AValCon "λ"

sigmaCon :: Doc
sigmaCon = PP.annotate' ATyCon "Σ"

prodCon, leftElimProd, rightElimProd :: Doc
prodCon = PP.annotate' ATyCon "×"
leftElimProd = PP.annotate' AValCon "π₁"
rightElimProd = PP.annotate' AValCon "π₂"

sumCon, leftIntroSum, rightIntroSum, elimSum :: Doc
sumCon = PP.annotate' ATyCon "+"
leftIntroSum = PP.annotate' AValCon "inl"
rightIntroSum = PP.annotate' AValCon "inr"
elimSum = PP.annotate' AValCon "case"

unitCon :: Doc
unitCon = PP.annotate' ATyCon "Unit"

unitVal :: Doc
unitVal = PP.annotate' ATyCon "⋆"

letVal :: Doc
letVal = PP.annotate' AValCon "let"

inVal :: Doc
inVal = PP.annotate' AValCon "in"

------------------------------------------------------------------------------
-- Getters
------------------------------------------------------------------------------

getBinds :: Term primTy primVal -> WithBinders (Term primTy primVal)
getBinds = go []
  where
    go ::
      [Binder (Term primTy primVal)] ->
      Term primTy primVal ->
      WithBinders (Term primTy primVal)
    go acc (Pi π x s t) = go (Binder PI π x s : acc) t
    go acc (Sig π x s t) = go (Binder SIG π x s : acc) t
    go acc t = (reverse acc, t)

getLams :: Term primTy primVal -> ([NameSymbol.T], Term primTy primVal)
getLams = go []
  where
    go acc (Lam x t) = go (x : acc) t
    go acc t = (reverse acc, t)

-- | Extract a right-nested tuple.
getPairs :: Term primTy primVal -> [Term primTy primVal]
getPairs (Pair s t) = s : getPairs t
getPairs t = [t]

getApps :: Elim primTy primVal -> (Elim primTy primVal, [Term primTy primVal])
getApps = go []
  where
    go acc (App f s) = go (s : acc) f
    go acc e = (e, reverse acc)

------------------------------------------------------------------------------
-- Doc printer functions
------------------------------------------------------------------------------

-- Print a sequence of binders:
--
-- @
-- Π (0 | A : * 0) → Π (1 | x : A) → Σ (1 | y : A) → P x y
-- -- or --
-- Π (0 | A : * 0) →
-- Π (1 | x : A) →
-- Σ (1 | y : A) →
--   P x y
-- @
ppBinders ::
  (PP.PrettySyntax tm, PP.Ann tm ~ PPAnn, PP.PrecReader m) =>
  WithBinders tm ->
  m Doc
ppBinders (bs, t) =
  PP.hangA PP.indentWidth (PP.sepA $ map ppBinder1 bs) (ppOuter t)
  where
    ppBinder1 (Binder b π x s) =
      PP.hsepA
        [ ppBind b,
          parens
            <$> PP.sepA
              [ PP.hsepA [ppUsage π, pure pipe],
                pure $ PP.hsep [name x, colon],
                ppOuter s
              ],
          pure arrow
        ]
    ppBind = pure . \case PI -> piCon; SIG -> sigmaCon

-- | Print a usage highlighted as a builtin value.
ppUsage :: PP.PrecReader m => Usage.T -> m Doc
ppUsage = fmap (PP.annotate' AValCon . PP.noAnn) . PP.pretty'

-- Print a sequence of lambdas:
--
-- @
-- λ x y z → x z (y z)
-- -- or --
-- λ x y z →
--     x z (y z)
-- -- or --
-- λ verylongname1
--   verylongname2
--   verylongname3 →
--     blah
-- @
ppLams ::
  (PP.PrettySyntax tm, PP.Ann tm ~ PPAnn, PP.PrecReader m) =>
  ([NameSymbol.T], tm) ->
  m Doc
ppLams (names, body) =
  PP.hangA (2 * PP.indentWidth) (pure header) (ppOuter body)
  where
    header = PP.hsep [lambda, ppNames names, arrow]
    ppNames = PP.sep . map (PP.noAnn . PP.prettyT)

-- | Print something with the loosest precedence (e.g. for the body of
-- a lambda).
ppOuter :: (PP.PrecReader m, PP.PrettySyntax a) => a -> m (PP.Doc (PP.Ann a))
ppOuter = PP.withPrec PP.Outer . PP.pretty'

-- | Print an application (see 'PP.app').
ppApps ::
  ( PP.PrettySyntax a,
    PP.Ann a ~ PPAnn,
    PP.PrettySyntax b,
    PP.Ann b ~ PPAnn,
    PP.PrecReader m
  ) =>
  a ->
  [b] ->
  m Doc
ppApps f xs = PP.app' APunct (PP.pretty' f) $ map PP.pretty' xs

-- | Print a tuple with commas and angle brackets.
ppPairs ::
  (PP.PrettySyntax a, PP.PrecReader m, PP.Ann a ~ PPAnn) =>
  [a] ->
  m Doc
ppPairs =
  fmap (angles . PP.sep . PP.punctuate comma) . traverse ppOuter

-- Product stuff

ppCatProduct = notImplemented

ppCatProductIntro :: (PP.PrecReader m, PP.PrettySyntax primTy, PP.PrettySyntax primVal, ToPPAnn (PP.Ann primTy), ToPPAnn (PP.Ann primVal)) => Term primTy primVal -> m Doc
ppCatProductIntro = ppPairs . getPairs

ppCatProductElimLeft = notImplemented

ppCatProductElimRight = notImplemented

-- Coproduct stuff

ppCatCoproduct = notImplemented

ppCatCoproductIntroLeft = notImplemented

ppCatCoproductIntroRight = notImplemented

ppCatCoproductElim :: (PP.PrecReader m, PP.PrettySyntax primTy, PP.PrettySyntax primVal, ToPPAnn (PP.Ann primTy), ToPPAnn (PP.Ann primVal)) => Term primTy primVal -> m Doc
ppCatCoproductElim = ppPairs . getPairs

-- | Print a 'let':
--
-- @
-- let 1 | x = expr in body
-- -- or --
-- let 1 | x = expr in
-- body
-- -- or --
-- let 1 | x =
--   longexpr in
-- body
-- @
ppLet ::
  ( PP.PrettySyntax a,
    PP.Ann a ~ PPAnn,
    PP.PrettySyntax b,
    PP.Ann b ~ PPAnn,
    PP.PrecReader m
  ) =>
  Usage.T ->
  NameSymbol.T ->
  a ->
  b ->
  m Doc
ppLet π x b t =
  PP.sepA
    [ PP.hsepA
        [ PP.hangA
            PP.indentWidth
            (PP.hsepA [let_, ppUsage π, pure pipe, pname x, pure equals])
            (ppOuter b),
          in_
        ],
      ppOuter t
    ]
  where
    let_ = pure letVal
    in_ = pure inVal

-- | Print a star and universe level highlighted as a builtin type.
ppStar :: PP.PrecReader m => Core.Universe -> m Doc
ppStar i = PP.app' APunct (pure universe) [pure . levelUniverse $ i]

------------------------------------------------------------------------------
-- PrimPretty instances
------------------------------------------------------------------------------

instance PrimPretty primTy primVal => PP.PrettySyntax (Term primTy primVal) where
  pretty' = \case
    -- Universe types
    Star i -> ppStar i
    -- Primitive types
    PrimTy ty -> PP.annotate' APrimTy . fmap toPPAnn <$> PP.pretty' ty
    Prim val -> PP.annotate' APrimVal . fmap toPPAnn <$> PP.pretty' val
    -- Pi-types
    t@Pi {} -> ppBinders $ getBinds t -- TODO: a simpler implementation of ppBinders without need of getBinds is probably better.
    t@(Lam _ _) -> ppLams $ getLams t
    -- Sigma-types
    t@Sig {} -> ppBinders $ getBinds t
    t@(Pair _ _) -> ppPairs $ getPairs t
    -- Product types
    t@CatProduct {} -> ppCatProduct t
    t@CatProductIntro {} -> ppCatProductIntro t
    t@CatProductElimLeft {} -> ppCatProductElimLeft t
    t@CatProductElimRight {} -> ppCatProductElimRight t
    -- Coproduct types
    t@CatCoproduct {} -> ppCatCoproduct t
    t@CatCoproductIntroLeft {} -> ppCatCoproductIntroLeft t
    t@CatCoproductIntroRight {} -> ppCatCoproductIntroRight t
    t@CatCoproductElim {} -> ppCatCoproductElim t
    -- Unit type
    UnitTy -> pure unitCon
    Unit -> pure box
    Let π x b t -> ppLet π x b t
    Elim e -> PP.pretty' e

instance PrimPretty primTy primVal => PP.PrettySyntax (Elim primTy primVal) where
  pretty' = \case
    Var x -> pname x
    App f s -> ppApps f' $ ss <> [s]
      where
        (f', ss) = getApps f
    Ann π s a ℓ ->
      parens
        <$> PP.hangsA
          PP.indentWidth
          (PP.hsepA [ppUsage π, pure pipe, ppOuter s])
          [ PP.hsepA [pure colon, ppOuter a],
            PP.hsepA [pure colon, ppStar ℓ]
          ]

instance
  PrimPretty primTy primVal =>
  PP.PrettySyntax (Pattern primTy primVal)
  where
  pretty' = \case
    PCon k ps -> PP.app' APunct (pname k) (map PP.pretty' ps)
    PPair a b -> ppPairs [a, b]
    PUnit -> pure box
    PVar x -> pname x
    PDot s -> PP.hsepA [pure dot, PP.withPrec PP.FunArg $ PP.pretty' s]
    PPrim p -> fmap toPPAnn <$> PP.pretty' p

class ToPPAnn ann where
  toPPAnn :: ann -> PPAnn

instance ToPPAnn () where
  toPPAnn () = mempty

instance ToPPAnn PPAnn where
  toPPAnn = identity
