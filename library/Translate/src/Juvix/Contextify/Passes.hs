module Juvix.Contextify.Passes
  ( resolveModule,
    inifixSoloPass,
  )
where

import Control.Lens hiding (op, (|>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Closure as Closure
import qualified Juvix.Context as Context
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.CoreNamed as StructureCore
import qualified Juvix.Sexp.Structure.Frontend as Structure
import Juvix.Sexp.Structure.Lens
import qualified StmContainers.Map as STM

--------------------------------------------------------------------------------
-- Environmental Constraint Types
--------------------------------------------------------------------------------

resolveRecordDeclaration ::
  Expression m => Env.SexpContext -> m Env.SexpContext
resolveRecordDeclaration context =
  Env.passContextExplicitBinderHandler
    -- we wish to register the name of the type we are working under
    Env.searchAndClosureRegisterTypeName
    context
    ( \x -> undefined -- x == Structure.nameRecordDec
    )
    (conPass registerRecordDeclaration)
  where
    conPass f = Env.Pass f f f

type ExpressionIO m = (Env.ErrS m, Env.HasClosure m, MonadIO m)

type Expression m = (Env.ErrS m, Env.HasClosure m)

--------------------------------------------------------------------------------
-- Open Transformation
--------------------------------------------------------------------------------

-- The resolveModule pass, looks over any S-expression, searching for
-- Atoms or open forms. In the case for Atoms, we see if there is any
-- module that it should be qualified to.

-- So for example if we have @sig foo : int -> int@ the @int@ atom
-- really belongs to perhaps @Prelude.Michelson.int@. Thus we will do
-- the conversion there. Further we change the @->@ atom into
-- @Prelude.->@ for the same reason.

-- In the open case we note the open into the closure, and then remove
-- the open from the source code

-- - BNF input form:
--   1. (:open-in Foo body)
--   2. unqualified-foo
-- - BNF output form:
--   1. body
--   2. qualified-foo

-- Note that the qualification of foo to Bar.foo if the symbol is from
-- an open module

resolveModule ::
  ExpressionIO m => Env.SexpContext -> m Env.SexpContext
resolveModule context =
  Env.passContextSingle context (\x -> x == ":atom" || x == Structure.nameOpenIn) openResolution

openResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.Atom -> Sexp.T -> m Sexp.T
openResolution ctx a cdr
  | Just open <- Structure.toOpenIn (Sexp.Cons (Sexp.Atom a) cdr) =
    pure (open ^. body)
  | otherwise =
    atomResolution ctx a cdr

atomResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.Atom -> Sexp.T -> m Sexp.T
atomResolution context atom@Sexp.A {atomName = name} sexpAtom = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just Closure.Info {mOpen = Just prefix} ->
      -- we qualified it to a module which is already qualified
      pure (Sexp.addMetaToCar atom (Sexp.atom (prefix <> name)))
    Just Closure.Info {} -> pure sexpAtom
    Nothing -> do
      let qualified = context ^. Context._currentNameSpace . Context.qualifiedMap
      looked <- liftIO $ atomically $ STM.lookup symbolName qualified
      case looked of
        Just Context.SymInfo {mod = prefix} ->
          pure $ Sexp.addMetaToCar atom (Sexp.atom (prefix <> name))
        Nothing -> pure sexpAtom
atomResolution _ _ s = pure s

--------------------------------------------------------------------------------
-- Infix Form Transformation
--------------------------------------------------------------------------------

inifixSoloPass ::
  Expression m => Env.SexpContext -> m Env.SexpContext
inifixSoloPass context =
  Env.passContextSingle context (== Structure.nameInfix) infixConversion

infixConversion ::
  (Env.ErrS m, Env.HasClosure m) => Context.T t y s -> Sexp.Atom -> Sexp.T -> m Sexp.T
infixConversion context atom list = do
  grouped <- groupInfix context (Sexp.Cons (Sexp.Atom atom) list)
  case Shunt.shunt grouped of
    Right shunted ->
      pure $ convertShunt shunted
    Left (Shunt.Clash pred1 pred2) ->
      throw @"error" (Env.Clash pred1 pred2)
    Left Shunt.MoreEles ->
      throw @"error" Env.ImpossibleMoreEles

registerRecordDeclaration ctx a cdr
  | Just type' <- Structure.toType form =
    registerCurrentForm type'
  | otherwise =
    undefined
  where
    form = Sexp.Cons (Sexp.Atom a) cdr

registerCurrentForm type' =
  pure (Structure.fromType type')

------------------------------------------------------------
-- Helpers for infix conversion
------------------------------------------------------------

groupInfix ::
  (Env.ErrS m, Env.HasClosure m) =>
  Context.T t y s ->
  Sexp.T ->
  m (NonEmpty (Shunt.PredOrEle Sexp.T Sexp.T))
groupInfix context xs
  | Just inf <- Structure.toInfix xs,
    Just Sexp.A {atomName = opSym} <- Sexp.atomFromT (inf ^. op) = do
    prec <- Env.lookupPrecedence opSym context
    moreInfixs <- groupInfix context (inf ^. right)
    precedenceConversion (inf ^. op) prec
      |> Shunt.Precedence
      |> flip NonEmpty.cons moreInfixs
      -- we cons l and not r, as "3 + 4 + 5 * 6"
      -- parses as "3 + (4 + (5 * 6))"
      -- thus the left is always an element
      |> NonEmpty.cons (Shunt.Ele (inf ^. left))
      |> pure
  | otherwise =
    pure (Shunt.Ele xs :| [])

precedenceConversion ::
  Sexp.T -> Context.Precedence -> Shunt.Precedence Sexp.T
precedenceConversion s (Context.Pred Context.Left i) =
  Shunt.Pred s Shunt.Left' i
precedenceConversion s (Context.Pred Context.Right i) =
  Shunt.Pred s Shunt.Right' i
precedenceConversion s (Context.Pred Context.NonAssoc i) =
  Shunt.Pred s Shunt.NonAssoc i

convertShunt :: Shunt.Application Sexp.T Sexp.T -> Sexp.T
convertShunt (Shunt.Single e) = e
convertShunt (Shunt.App s app1 app2) =
  Sexp.list [s, convertShunt app1, convertShunt app2]

--------------------------------------------------------------------------------
-- Record Transform
--------------------------------------------------------------------------------

-- Update this…

-- Currently we aren't running a free variable algorithm on this to
-- determine dependency order. This should be run to ensure that we are
-- getting the order right. Also we need to pick a canonical ordering

-- | @recordPi@ - Transforms record declarations into @Pi@ Type
-- expressions
-- - BNF Input Form
--   1. (:record-d arg-1 type-1 … arg-n type-n)
-- - BNF Output Form
--   1. (:sigma (arg-1 :usage-hole type-1)
--         (:sigma (arg-2 :usage-hole type-2)
--            … (:sigma (arg-n :usage-hole type-n)
--                  :unit)))
recordPi ::
  Expression m => Env.SexpContext -> m Env.SexpContext
recordPi context =
  Env.passContextSingle context (== Structure.nameRecordDec) removeRecordD
  where
    removeRecordD _context atom cdr =
      case Structure.toRecordDec (Sexp.Atom atom Sexp.:> cdr) of
        Just recordD ->
          -- we are doing a lookup, but here is where we do our sorting
          recordD ^. value
            |> foldrM piTransform (Sexp.atom ":unit")
        Nothing -> throw @"error" (Env.ImproperForm "malformed record declaration")
      where
        piTransform termType sigmaChain
          | Just name <- Sexp.nameFromT (termType ^. name) =
            sigmaChain
              |> StructureCore.Sigma
                (StructureCore.Binder name (Sexp.atom ":omega") (termType ^. value))
              |> StructureCore.fromSigma
              |> pure
          | otherwise =
            throw @"error" (Env.ImproperForm "malformed record declaration name")
