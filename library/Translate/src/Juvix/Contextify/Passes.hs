{-# LANGUAGE ViewPatterns #-}
module Juvix.Contextify.Passes
  ( resolveModule,
    inifixSoloPass,
    recordDeclaration,
    notFoundSymbolToLookup,
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
import qualified Juvix.Sexp.Structure.CoreNamed as CoreNamed
import qualified Juvix.Sexp.Structure.Helpers as CoreNamed
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified StmContainers.Map as STM

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
  Env.contextPassStar
    context
    (== Structure.nameOpenIn)
    (mempty {Sexp.onAtom = True})
    openResolution

openResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.T -> m Sexp.T
openResolution _ctx (Structure.toOpenIn -> Just open) =
    pure (open ^. body)
openResolution ctx sexp =
    atomResolution ctx sexp

atomResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.T -> m Sexp.T
atomResolution context sexpAtom@(Sexp.Atom (atom@Sexp.A {atomName = name})) = do
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
atomResolution _ s = pure s

--------------------------------------------------------------------------------
-- Infix Form Transformation
--------------------------------------------------------------------------------

-- TODO ∷ add comment about infixl vs infix vs infixr, and explain how
-- this isn't a real bnf

-- | @infixSoloPass@ resolves the infix precedence of a given form into
-- a properly ordered prefix ordering via the shuntyard algorithm.
-- - BNF input form:
--   1. (:infix infix-3 3 (:infix infix-4 1 (:infix infix-2 5 7)))
-- - BNF output form:
--   1. (:infix-3 3 (:infix-2 (:infix-4 1 5) 7))
-- - Note :: infix-<num> stands for precedent <num>
inifixSoloPass ::
  Expression m => Env.SexpContext -> m Env.SexpContext
inifixSoloPass context =
  Env.contextPassStar context (== Structure.nameInfix) mempty infixConversion

infixConversion ::
  (Env.ErrS m, Env.HasClosure m) => Context.T t y s -> Sexp.T -> m Sexp.T
infixConversion context sexp = do
  grouped <- groupInfix context sexp
  case Shunt.shunt grouped of
    Right shunted ->
      pure $ convertShunt shunted
    Left (Shunt.Clash pred1 pred2) ->
      throw @"error" (Env.Clash pred1 pred2)
    Left Shunt.MoreEles ->
      throw @"error" Env.ImpossibleMoreEles

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
-- Record Recognition Transformation
--------------------------------------------------------------------------------

recordDeclaration ::
  ExpressionIO m => Env.SexpContext -> m Env.SexpContext
recordDeclaration context =
  Env.contextPassChange context (== Structure.nameType) figureRecord

-- - input form
--   1. (type name₁ (arg₁ … argₙ)
--        (:record-d (field₁ usage₁ type₁)
--                   …
--                   (fieldₙ usageₙ typeₙ)))
-- - output form
--   1. Def
--      { type = Nothing
--      , def  = (:defsig-match name₁ ()
--                  ((arg₁ … argₙ)
--                   (:record-ty (field₁ usage₁ type₁)
--                                …
--                               (fieldₙ usageₙ typeₙ))))
--      }
figureRecord ::
  ExpressionIO m => Env.PassChange m
figureRecord = Env.PassChange rec
  where
    rec _ctx (Structure.toType -> Just type') defName
      | -- make sure it's a record only declaration
        -- how do we handle sum types?
        maybe 0 length (Sexp.toList (type' ^. body)) == 1,
        Just record <- Structure.toRecordDec (Sexp.car (type' ^. body)) =
        recordToFields record
          >>| CoreNamed.RecordTy
          >>| CoreNamed.fromRecordTy
          >>| Structure.ArgBody (type' ^. args)
          >>| (: [])
          >>| Structure.LambdaCase
          >>| Structure.fromLambdaCase
          >>| ( \arg ->
                  Context.D
                    { defUsage = Nothing,
                      defMTy = Nothing,
                      defTerm = arg,
                      defPrecedence = Context.default'
                    }
              )
          >>| Context.Def
          >>| \x -> Just (defName, x)
    rec _ _ _ =
        pure Nothing

------------------------------------------------------------
-- Structure Conversion Helpers
------------------------------------------------------------

recordToFields ::
  (HasThrow "error" Env.ErrorS m) => Structure.RecordDec -> m [CoreNamed.Field]
recordToFields record =
  traverse notPunnedToField (record ^. value)

notPunnedToField ::
  (HasThrow "error" Env.ErrorS m) => Structure.NameUsage -> m CoreNamed.Field
notPunnedToField notPunned = do
  name <- sexpToNameSymbolErr (notPunned ^. name)
  pure $ CoreNamed.Field name (notPunned ^. usage) (notPunned ^. value)

sexpToNameSymbolErr ::
  HasThrow "error" Env.ErrorS m => Sexp.T -> m NameSymbol.T
sexpToNameSymbolErr sexp =
  case Sexp.atomFromT sexp of
    Nothing ->
      throw @"error" (Env.ImproperForm sexp)
    Just form ->
      pure (Sexp.atomName form)

------------------------------------------------------------
-- Record Lookup from Unknown Symbols
------------------------------------------------------------

-- We expect that symbol resolution has already run.
-- - Input BNF
--   1. module-name₁.module-name₂.….name-in-ctx₁.name₁.….nameₙ
--   2. module-name₁.module-name₂.….name-in-ctx₁
--   3. (:let-match foo (() (:record-no-pun x (:record-no-pun y 3)))
--         foo.x.y)
-- - Output BNF
--   1. (:lookup (module-name₁.module-name₂.….name-in-ctx₁) (name₁ … nameₙ))
--   2. module-name₁.module-name₂.….name-in-ctx₁
--   3. (:let-match foo (() (:record-no-pun x (:record-no-pun y 3)))
--         (:lookup foo (x y)))
notFoundSymbolToLookup ::
  ExpressionIO m => Env.SexpContext -> m Env.SexpContext
notFoundSymbolToLookup context =
  Env.contextPassManaulStar
    context
    (== Structure.namePrimitive)
    mempty {Sexp.onAtom = True}
    (primiveOrSymbol)

primiveOrSymbol ::
  ExpressionIO m => Env.SexpContext -> Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
primiveOrSymbol context sexp _rec'
  | Just _struct <- Structure.toPrimitive sexp =
    pure sexp
  | otherwise =
    notFoundAtomRes context sexp

notFoundAtomRes ::
  ExpressionIO m => Env.SexpContext -> Sexp.T -> m Sexp.T
notFoundAtomRes context sexpAtom@(Sexp.Atom atom@Sexp.A {atomName = name}) = do
  -- we need to check if the first part of the var is in our
  closure <- ask @"closure"
  let firstQualification :| symbols = NameSymbol.toNonEmptySymbol name
      generateLookup lookupPath termInQuestion =
        case lookupPath of
          [] ->
            termInQuestion
          syms ->
            CoreNamed.Lookup termInQuestion syms |> CoreNamed.fromLookup
          |> Sexp.addMetaToCar atom
          |> pure
  case Closure.lookup firstQualification closure of
    Just Closure.Info {} ->
      -- If @symbols@ are not empty then it must be a record lookup so
      -- let foo = {x = 3} in foo.x
      generateLookup symbols (CoreNamed.fromSymbol firstQualification)
    Nothing ->
      -- In this case the symbol is not allocated into the Closure, and
      -- so may be in the context. We need to figure out how =.='s
      -- until we hit a non record in the context.

      -- For this we will do a little trick, namely we start to lookup
      -- Names until we hit a resolution so for example if we have
      -- =Foo.Bar.Baz.zed= where Bar is a Def in the context then we
      -- try =Foo.Bar.Baz.zed= then =Foo.Bar.Baz=, then finally
      -- =Foo.Bar= which should resolve.
      let lastLookup =
            (firstQualification : symbols)
              |> NonEmpty.inits
              |> NonEmpty.tail
              |> reverse
              >>| second (`Context.lookup` context) . dup . NonEmpty.fromList
              |> find (\(_, maybe) -> isJust maybe)
       in case lastLookup of
            -- In this case we have a definition that we've resolved,
            -- the last resolved symbol is @lastName@. Note that if
            -- @lastName@ ≈ @name@, then and we will end up with this
            -- pass doing nothing. If this is not the case then the
            -- symbols given back by @NameSymbol.takePrefixOfInternal@
            -- are the lookup.

            -- λ> NameSymbol.takePrefixOfInternal
            --       (NameSymbol.fromSymbol "Hi.Bar")
            --       (NameSymbol.fromSymbol "Hi.Bar.Baz.foo")
            -- Just ["Baz","foo"]
            -- λ> NameSymbol.takePrefixOfInternal
            --       (NameSymbol.fromSymbol "Hi.Bar.Baz.foo")
            --       (NameSymbol.fromSymbol "Hi.Bar.Baz.foo")
            -- Just []
            Just (lastName, _) -> do
              CoreNamed.fromNameSymbol lastName
                |> generateLookup
                  (maybe [] identity (NameSymbol.takePrefixOfInternal lastName name))
            -- In this case we can't resolve the symbol at all! It
            -- seems to be not defined at all, as it's not in the
            -- Context nor the closure, let's just return back what is given to us!
            Nothing ->
              pure sexpAtom
notFoundAtomRes _ s = pure s
