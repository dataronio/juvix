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
import qualified Juvix.Contextify.Binders as Bind
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
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
    openResolution

openResolution ::
  ExpressionIO m => Env.Pass m ()
openResolution ctx atom rec' = do
  -- this will do the job for openIn, and nothing to NameSymbol
  res <- Env.handleAtom ctx atom rec'
  case res of
    Sexp.P (Bind.OpenIn _mod body) _ -> pure body
    Sexp.A _symbol _________________ -> atomResolution ctx res
    ________________________________ -> pure (Sexp.Atom res)

atomResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.Atom a -> m (Sexp.B a)
atomResolution context (atom@Sexp.A {atomName = name}) = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just Closure.Info {mOpen = Just prefix} ->
      -- we qualified it to a module which is already qualified
      pure (Sexp.addMetaToCar atom (Sexp.atom (prefix <> name)))
    Just Closure.Info {} -> pure (Sexp.Atom atom)
    Nothing -> do
      let qualified = context ^. Context._currentNameSpace . Context.qualifiedMap
      looked <- liftIO $ atomically $ STM.lookup symbolName qualified
      case looked of
        Just Context.SymInfo {mod = prefix} ->
          pure $ Sexp.addMetaToCar atom (Sexp.atom (prefix <> name))
        Nothing -> pure (Sexp.Atom atom)
atomResolution _ s = pure (Sexp.Atom s)

--------------------------------------------------------------------------------
-- Infix Form Transformation
--------------------------------------------------------------------------------

data Infix
  = Infix
      { infixOp :: NameSymbol.T,
        infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
        infixInf :: Infix
      }
  | InfixNoMore
      { infixOp :: NameSymbol.T,
        infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
        infixRt :: (Sexp.B (Bind.BinderPlus Infix))
      }
  deriving (Show, Generic, Eq)

infixRename :: Sexp.Options
infixRename =
  Sexp.changeName
    (Sexp.defaultOptions @Infix)
    (Map.fromList [("InfixNoMore", ":infix")])

instance Sexp.DefaultOptions Infix

instance Sexp.Serialize Infix where
  serialize = Sexp.serializeOpt infixRename
  deserialize = Sexp.deserializeOpt infixRename

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
  Env.contextPassStar @Infix context infixConversion

infixConversion ::
  (Env.ErrS m, Env.HasClosure m) => Env.Pass m Infix
infixConversion context atom rec' =
  case atom of
    Sexp.P (Bind.Other inf) _ -> do
      grouped <- groupInfix context inf
      case Shunt.shunt grouped of
        Right shunted ->
          rec' (convertShunt shunted)
        Left (Shunt.Clash pred1 pred2) ->
          throw @"error" (Env.Clash pred1 pred2)
        Left Shunt.MoreEles ->
          throw @"error" Env.ImpossibleMoreEles
    _ -> Env.handleAtom context atom rec' >>| Sexp.Atom

------------------------------------------------------------
-- Helpers for infix conversion
------------------------------------------------------------

data InfFlat a = Inf NameSymbol.T | Ele a

infixToInfFlat ::
  Infix -> NonEmpty (InfFlat (Sexp.B (Bind.BinderPlus Infix)))
infixToInfFlat (InfixNoMore op lt rt) =
  NonEmpty.fromList [Ele lt, Inf op, Ele rt]
infixToInfFlat (Infix op lt rt) =
  NonEmpty.fromList [Ele lt, Inf op] <> infixToInfFlat rt

groupInfix ::
  (Env.ErrS f, Env.HasClosure f) =>
  Context.T t y s ->
  Infix ->
  f (NonEmpty (Shunt.PredOrEle NameSymbol.T (Sexp.B (Bind.BinderPlus Infix))))
groupInfix context inf =
  traverse f (infixToInfFlat inf)
  where
    f (Ele a) = pure (Shunt.Ele a)
    f (Inf op) = do
      prec <- Env.lookupPrecedence op context
      pure (Shunt.Precedence (precedenceConversion op prec))

precedenceConversion ::
  NameSymbol.T -> Context.Precedence -> Shunt.Precedence NameSymbol.T
precedenceConversion s (Context.Pred Context.Left i) =
  Shunt.Pred s Shunt.Left' i
precedenceConversion s (Context.Pred Context.Right i) =
  Shunt.Pred s Shunt.Right' i
precedenceConversion s (Context.Pred Context.NonAssoc i) =
  Shunt.Pred s Shunt.NonAssoc i

convertShunt :: Shunt.Application NameSymbol.T (Sexp.B a) -> Sexp.B a
convertShunt (Shunt.Single e) = e
convertShunt (Shunt.App s app1 app2) =
  Sexp.list [Sexp.atom s, convertShunt app1, convertShunt app2]

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
  Env.contextPassStar
    context
    (primiveOrSymbol)

primiveOrSymbol ::
  (ExpressionIO m) => Env.Pass m ()
primiveOrSymbol context atom rec' =
  case atom of
    Sexp.P (Bind.Primitive _) _ ->
      Sexp.Atom atom |> pure
    Sexp.A {} ->
      notFoundAtomRes context atom
    _ -> Env.handleAtom context atom rec' >>| Sexp.Atom

notFoundAtomRes ::
  (Sexp.Serialize a, ExpressionIO m) => Env.SexpContext -> Sexp.Atom a -> m (Sexp.B a)
notFoundAtomRes context atom@Sexp.A {atomName = name} = do
  -- we need to check if the first part of the var is in our
  closure <- ask @"closure"
  let firstQualification :| symbols = NameSymbol.toNonEmptySymbol name
      generateLookup lookupPath termInQuestion =
        case lookupPath of
          [] ->
            termInQuestion
          syms ->
            CoreNamed.Lookup termInQuestion syms |> CoreNamed.fromLookup
          |> Sexp.partiallyDeserialize
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
              pure (Sexp.Atom atom)
notFoundAtomRes _ a = pure (Sexp.Atom a)
