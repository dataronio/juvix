module Juvix.Core.Pretty
  ( module Juvix.Library.PrettyPrint,
    PPAnn' (..),
    PPAnn,
    ToPPAnn (..),
    Doc,
    Doc',
    PrimPretty1,
    PrimPretty,
    PrettyText',

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
    unitCon,
    unitVal,
  )
where

import qualified Juvix.Core.Base as Core
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.PrettyPrint hiding (Doc, parens, parensP)
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage

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

-- | Requirements to be able to print a primitive type or value
type PrimPretty1 p = (PrettySyntax p, ToPPAnn (Ann p))

type PrimPretty ty val = (PrimPretty1 ty, PrimPretty1 val)

type PrettyText' a = (PrettyText a, ToPPAnn (Ann a))

-- | Document with syntax highlighting hints
type Doc = PP.Doc PPAnn

type Doc' = PP.Doc

-- | Surround with parens @(…)@.
parens :: Doc -> Doc
parens = parens' APunct

-- | Surround with parens if the current precedence level is greater than the
-- given one, as with 'Text.Show.showsPrec'.
parensP :: PrecReader m => Prec -> m Doc -> m Doc
parensP = parensP' APunct

-- | Surround with angle brackets @‹…›@.
angles :: Doc -> Doc
angles = annotate' APunct "‹" `enclose` annotate' APunct "›"

comma :: Doc
comma = annotate' APunct ","

dot :: Doc
dot = annotate' APunct "."

colon :: Doc
colon = annotate' APunct ":"

arrow :: Doc
arrow = annotate' APunct "→"

mapsto :: Doc
mapsto = annotate' APunct "↦"

equals :: Doc
equals = annotate' APunct "="

pipe :: Doc
pipe = annotate' APunct "|"

-- | ⌷ @APL FUNCTIONAL SYMBOL SQUISH QUAD@, used for printing the unit value
box :: Doc
box = annotate' AValCon "⌷"

-- | Print a @NameSymbol.‌'NameSymbol.T'@ in dotted format and highlighted
-- as a name.
name :: NameSymbol.T -> Doc
name = annotate' AName . string . unintern . NameSymbol.toSymbol

pname :: Applicative f => NameSymbol.T -> f Doc
pname = pure . name

-- TODO: Instead of * better if we use "υ"
universe :: Doc
universe = annotate' ATyCon "*"

-- TODO: Use subindices whenever it's possible, e.g. υ₁ or υ₂.
levelUniverse :: Core.Universe -> Doc
levelUniverse i = annotate' ATyCon $ case i of
  Core.U i -> PP.show i
  Core.UAny -> "_"

piCon :: Doc
piCon = annotate' ATyCon "Π"

lambda :: Doc
lambda = annotate' AValCon "λ"

sigmaCon :: Doc
sigmaCon = annotate' ATyCon "Σ"

prodCon, leftElimProd, rightElimProd :: Doc
prodCon = annotate' ATyCon "×"
leftElimProd = annotate' AValCon "π₁"
rightElimProd = annotate' AValCon "π₂"

sumCon, leftIntroSum, rightIntroSum, elimSum :: Doc
sumCon = annotate' ATyCon "+"
leftIntroSum = annotate' AValCon "inl"
rightIntroSum = annotate' AValCon "inr"
elimSum = annotate' AValCon "case"

unitCon :: Doc
unitCon = annotate' ATyCon "Unit"

unitVal :: Doc
unitVal = box -- TODO: It's better to have unit as ⋆ but it might be confused with the symbol for universes *. We leave it for the moment as the box symbol.

data Bind = PI | SIG

data Binder tm = Binder
  { bBinder :: Bind,
    bUsage :: Usage.T,
    bName :: NameSymbol.T,
    bType :: tm
  }

type WithBinders tm = ([Binder tm], tm)

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
  (PrettySyntax tm, Ann tm ~ PPAnn, PrecReader m) =>
  WithBinders tm ->
  m Doc
ppBinders (bs, t) =
  hangA indentWidth (sepA $ map ppBinder1 bs) (ppOuter t)
  where
    ppBinder1 (Binder b π x s) =
      hsepA $
        [ ppBind b,
          parens
            <$> sepA
              [ hsepA [ppUsage π, pure pipe],
                pure $ hsep [name x, colon],
                ppOuter s
              ],
          pure arrow
        ]
    ppBind = pure . \case PI -> piCon; SIG -> sigmaCon

-- | Print a usage highlighted as a builtin value.
ppUsage :: PrecReader m => Usage.T -> m Doc
ppUsage = fmap (annotate' AValCon . noAnn) . pretty'

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
  (PrettySyntax tm, Ann tm ~ PPAnn, PrecReader m) =>
  ([NameSymbol.T], tm) ->
  m Doc
ppLams (names, body) =
  hangA (2 * indentWidth) (pure header) (ppOuter body)
  where
    header = hsep [lambda, ppNames names, arrow]
    ppNames = sep . map (noAnn . prettyT)

-- | Print something with the loosest precedence (e.g. for the body of
-- a lambda).
ppOuter :: (PrecReader m, PrettySyntax a) => a -> m (PP.Doc (Ann a))
ppOuter = withPrec Outer . pretty'

-- | Print an application (see 'app').
ppApps ::
  ( PrettySyntax a,
    ToPPAnn (Ann a),
    PrettySyntax b,
    ToPPAnn (Ann b),
    PrecReader m,
    Foldable t
  ) =>
  a ->
  t b ->
  m Doc
ppApps f xs =
  app' APunct (fmap toPPAnn <$> pretty' f) $
    map (fmap (fmap toPPAnn) . pretty') $ toList xs

-- | Print a tuple with commas and angle brackets.
ppPairs ::
  (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) =>
  [a] ->
  m Doc
ppPairs =
  fmap (angles . sep . punctuate comma) . traverse ppOuter

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
  ( PrettySyntax a,
    Ann a ~ PPAnn,
    PrettySyntax b,
    Ann b ~ PPAnn,
    PrecReader m
  ) =>
  Usage.T ->
  NameSymbol.T ->
  a ->
  b ->
  m Doc
ppLet π x b t =
  sepA
    [ hsepA
        [ hangA
            indentWidth
            (hsepA [let_, ppUsage π, pure pipe, pname x, pure equals])
            (ppOuter b),
          in_
        ],
      ppOuter t
    ]
  where
    let_ = pure $ annotate' AValCon "let"
    in_ = pure $ annotate' AValCon "in"

-- | Print a star and universe level highlighted as a builtin type.
ppStar :: PrecReader m => Core.Universe -> m Doc
ppStar i = app' APunct (pure universe) [pure $ levelUniverse i]

-- | Print a product type e.g. (S × T).
ppCatProduct :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> a -> m Doc
ppCatProduct s t = fmap parens (hsepA [pretty' s, pure prodCon, pretty' t])

-- | Print a term of a product: (s , t).
ppCatProductIntro :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> a -> m Doc
ppCatProductIntro t1 t2 =
  fmap parens (hsepA [pretty' t1, pure comma, pretty' t2])

-- | Print the left eliminator of a product.
ppCatProductElimLeft :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> m Doc
ppCatProductElimLeft t = fmap parens (hsepA [pure leftElimProd, pretty' t])

-- | Print the left eliminator of a product.
ppCatProductElimRight :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> m Doc
ppCatProductElimRight t = fmap parens (hsepA [pure rightElimProd, pretty' t])

-- | Print a product type e.g. (S + T).
ppCatCoproduct :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> a -> m Doc
ppCatCoproduct s t = fmap parens (hsepA [pretty' s, pure sumCon, pretty' t])

-- | Print a term as the left inj of a sum type e.g. inj₁ t.
ppCatCoproductIntroLeft :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> m Doc
ppCatCoproductIntroLeft t = fmap parens (hsepA [pure leftIntroSum, pretty' t])

-- | Print a term as the right inj of a sum type e.g. inj₂ t.
ppCatCoproductIntroRight :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> m Doc
ppCatCoproductIntroRight t = fmap parens (hsepA [pure rightIntroSum, pretty' t])

-- | Print an elimination of a coproduct term a.k.a. case-eliminator.
ppCatCoproductElim :: (PrettySyntax a, PrecReader m, Ann a ~ PPAnn) => a -> a -> a -> m Doc
ppCatCoproductElim match c1 c2 = hangsA indentWidth caseDoc [inl, inr]
  where
    caseDoc = hsepA [pure elimSum, pretty' match, pure arrow]
    inl = hsepA [pure leftIntroSum, pure mapsto, pretty' c1]
    inr = hsepA [pure leftIntroSum, pure mapsto, pretty' c2]

class ToPPAnn ann where
  toPPAnn :: ann -> PPAnn

instance ToPPAnn () where
  toPPAnn () = mempty

instance ToPPAnn PPAnn where
  toPPAnn = identity
