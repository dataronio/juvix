{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Contextify.Environment
  ( ErrorS (..),
    ErrS,
    SexpContext,
    HasClosure,
    contextPassStar,
    contextPassChange,
    contextPassManaulStar,
    extractInformation,
    lookupPrecedence,
    Minimal (..),
    MinimalAlias,
    MinimalAliasIO,
    MinimalM (..),
    MinimalMIO (..),
    runMIO,
    runM,
    namedForms,
    onExpression,
    singlePassAuto,
    PassChange (..),
  )
where

import Control.Lens hiding ((|>))
import qualified Juvix.Closure as Closure
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Traversal as Context
import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library hiding (on)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import Prelude (error)

data ErrorS
  = CantResolve [Sexp.T]
  | UnknownSymbol NameSymbol.T
  | ImproperForm Sexp.T
  | ImpossibleMoreEles
  | Clash
      (Shunt.Precedence Sexp.T)
      (Shunt.Precedence Sexp.T)
  deriving (Show, Eq)

type HasClosure m = HasReader "closure" Closure.T m

type ErrS m = HasThrow "error" ErrorS m

type HasSearch m = (ErrS m, HasClosure m)

------------------------------------------------------------
-- Runner environment
------------------------------------------------------------

newtype Minimal = Minimal
  { closure :: Closure.T
  }
  deriving (Generic, Show)

type MinimalAlias =
  ExceptT ErrorS (State Minimal)

newtype MinimalM a = Ctx {_run :: MinimalAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAlias
  deriving
    (HasThrow "error" ErrorS)
    via MonadError MinimalAlias

type MinimalAliasIO =
  ExceptT ErrorS (StateT Minimal IO)

newtype MinimalMIO a = CtxIO {_runIO :: MinimalAliasIO a}
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAliasIO
  deriving
    (HasThrow "error" ErrorS)
    via MonadError MinimalAliasIO

runMIO :: MinimalMIO a -> IO (Either ErrorS a, Minimal)
runMIO (CtxIO c) = runStateT (runExceptT c) (Minimal Closure.empty)

runM :: MinimalM a -> (Either ErrorS a, Minimal)
runM (Ctx c) = runState (runExceptT c) (Minimal Closure.empty)

------------------------------------------------------------
-- Type aliases
------------------------------------------------------------

type SexpContext = Context.T Sexp.T Sexp.T Sexp.T

type SexpForms m =
  Context.ContextForms m Sexp.T Sexp.T Sexp.T

type SexpFormsGeneral m =
  Context.ContextFormGeneral m Sexp.T Sexp.T Sexp.T

type Pred = NameSymbol.T -> Bool

type PassAuto m =
  SexpContext -> Sexp.T -> m (Sexp.T)

type PassManual m =
  SexpContext -> Sexp.T -> (Sexp.T -> m (Sexp.T)) -> m (Sexp.T)

-- | @PassChange@ Instead of recursing on the sexp structure, we check
-- the top form and see if it needs to be changed.
newtype PassChange m
  = PassChange
      ( SexpContext ->
        Sexp.T ->
        NameSpace.From Symbol ->
        m (Maybe (NameSpace.From Symbol, Context.Definition Sexp.T Sexp.T Sexp.T))
      )
  deriving (Show)

------------------------------------------------------------
-- Main Functionality
------------------------------------------------------------

-- | @passContext@ like @passContextSingle@ but we supply a different
-- function for each type term and sum representation form.
contextPassStar ::
  HasSearch m => SexpContext -> Pred -> Sexp.Opt () () -> PassAuto m -> m SexpContext
contextPassStar ctx trigger opt pass =
  Context.mapWithContext ctx (singlePassAuto trigger opt pass)

contextPassManaulStar ::
  HasSearch m => SexpContext -> Pred -> Sexp.Opt () () -> PassManual m -> m SexpContext
contextPassManaulStar ctx trigger opt pass =
  Context.mapWithContext ctx (singlePassManual trigger opt pass)

contextPassChange ::
  Monad m => SexpContext -> Pred -> PassChange m -> m SexpContext
contextPassChange ctx trigger pass =
  Context.mapWithContext ctx (singlePassChange trigger pass)

-- | @onExpression@ runs an algorithm similar to @passContext@ however
-- only on a single expression instead of an entire context. For this
-- to have the closure work properly, please run this after :open-in is
-- gone
onExpression ::
  HasClosure f => Sexp.T -> Pred -> Sexp.Opt () () -> (Sexp.T -> f Sexp.T) -> f Sexp.T
onExpression form trigger opt func =
  Sexp.traversePredOptStar form trig (binderDispatchWithNoCtxF (trigger, func)) opt
  where
    trig x = trigger x || bindingForms x

------------------------------------------------------------
-- Pass infrastructure
------------------------------------------------------------

singlePassManual ::
  HasSearch m => Pred -> Sexp.Opt () () -> PassManual m -> SexpForms m
singlePassManual trig opt pass =
  Context.CtxSingle (traverseBinderStar pass trig opt)
    |> Context.promote

singlePassAuto ::
  HasSearch m => Pred -> Sexp.Opt () () -> PassAuto m -> SexpForms m
singlePassAuto trig opt pass =
  singlePassManual trig opt (Sexp.autoRecurse . pass)

singlePassChange ::
  Monad m => Pred -> PassChange m -> SexpFormsGeneral m
singlePassChange trigger (PassChange f) =
  let onRepresentation sexp ctx Context.Extra {name}
        | Just atomName <- Sexp.nameFromT (Sexp.car sexp),
          trigger atomName =
          f ctx sexp name
        | otherwise = pure Nothing
   in Context.CtxChangeSumRep onRepresentation |> Context.promote

traverseBinderStar ::
  HasSearch f => PassManual f -> Pred -> Sexp.Opt () () -> Sexp.T -> SexpContext -> f Sexp.T
traverseBinderStar func trigger opt form ctx =
  Sexp.traversePredOptStar form trig (binderDispatchWith (trigger, func) ctx) opt
  where
    trig x = trigger x || bindingForms x

------------------------------------------------------------
-- Binding form infrastructure
------------------------------------------------------------

-- | @bindingForms@ is a predicate that answers true for every form
-- that instantiates a new variable
bindingForms :: (Eq a, IsString a) => a -> Bool
bindingForms x =
  x
    `elem` [ "type",
             ":open-in",
             ":let-type",
             ":let-match",
             "case",
             ":lambda-case",
             ":declaim",
             ":lambda",
             ":primitive",
             ":defop"
           ]

-- | @namedForms@ a list of all named special forms
namedForms :: [NameSymbol.T]
namedForms =
  [ "type",
    ":open-in",
    ":let-type",
    ":let-match",
    "case",
    ":lambda-case",
    ":declaim",
    ":lambda",
    ":tuple",
    ":primitive",
    ":progn",
    "declare",
    ":infix",
    ":list",
    ":record-no-pun",
    ":paren",
    ":block",
    ":primitive",
    ":defeff",
    ":defhandler",
    ":defop",
    ":opsig",
    ":via"
  ]

-- | @binderDispatchWith@ is responsible for properly updating the
-- closure based on any binders we may encounter as well as matching
-- and dispatching on any form the user wishes to
-- match. @binderDispatchWith@ does this by taking a tuple of the data
-- the user wishes to match on and the transformation function. This
-- function takes president over the matches.
binderDispatchWith ::
  (HasClosure f, ErrS f) =>
  -- | The tuple represents the closure match function
  (Pred, PassManual f) ->
  -- | The Context, an extra function that is required the by the
  -- :open-in case.
  SexpContext ->
  -- | The sexp form in which the atom is called on
  Sexp.T ->
  -- | the continuation of continuing the changes
  (Sexp.T -> f Sexp.T) ->
  f Sexp.T
binderDispatchWith (trig, func) ctx sexp@(atom Sexp.:> _) cont
  | Just a <- Sexp.nameFromT atom,
    trig a =
    func ctx sexp cont
  | Structure.isOpenIn sexp =
    openIn ctx sexp cont
binderDispatchWith (_trig, func) ctx atom@(Sexp.Atom _) cont =
  func ctx atom cont
binderDispatchWith _ _ sexp cont =
  binderDispatchWithNoCtx sexp cont

-- | @binderDispatchWithNoF@ works like @binderDispatch@ however the function passed
binderDispatchWithNoCtxF ::
  HasClosure f =>
  -- | The tuple represents the closure match function
  (Pred, (Sexp.T -> f Sexp.T)) ->
  -- | The sexp form in which the atom is called on
  Sexp.T ->
  -- | the continuation of continuing the changes
  (Sexp.T -> f Sexp.T) ->
  f Sexp.T
binderDispatchWithNoCtxF (trig, func) sexp@(atom Sexp.:> _as) cont
  | Just a <- Sexp.nameFromT atom,
    trig a =
    (Sexp.autoRecurse func) sexp cont
binderDispatchWithNoCtxF (_trig, func) atom@(Sexp.Atom _) cont =
  (Sexp.autoRecurse func) atom cont
binderDispatchWithNoCtxF _ sexp cont =
  binderDispatchWithNoCtx sexp cont

-- | @binderDispatchWithNoCtx@ like @binderDispatchWith@ but does not rely on
-- a context, thus the open pass can't be done.
binderDispatchWithNoCtx ::
  HasClosure f =>
  -- | The sexp form in which the atom is called on
  Sexp.T ->
  -- | the continuation of continuing the changes
  (Sexp.T -> f Sexp.T) ->
  f Sexp.T
binderDispatchWithNoCtx sexp cont
  | Structure.isCase sexp = case' sexp cont
  -- this case happens at the start of every defun
  | Structure.isLambdaCase sexp = lambdaCase sexp cont
  -- This case is a bit special, as we must check the context for
  -- various names this may introduce to the
  | Structure.isType sexp = type' sexp cont
  | Structure.isLambda sexp = lambda sexp cont
  | Structure.isHandler sexp = handler sexp cont
  | Structure.isDeclaim sexp = declaim sexp cont
  | Structure.isLetType sexp = letType sexp cont
  | Structure.isLetMatch sexp = letMatch sexp cont
  | Structure.isPrimitive sexp = primitive sexp cont
-- TODO ∷ defop handler!??!?
binderDispatchWithNoCtx _ _ = error "improper closure call"

------------------------------------------------------------
-- Environment functionality
------------------------------------------------------------
extractInformation ::
  Context.Definition term ty sumRep -> Maybe [Context.Information]
extractInformation (Context.Def Context.D {defPrecedence}) =
  Just [Context.Prec defPrecedence]
extractInformation (Context.Information is) =
  Just is
extractInformation _ = Nothing

lookupPrecedence ::
  (ErrS m, HasClosure m) => NameSymbol.T -> Context.T t y s -> m Context.Precedence
lookupPrecedence name ctx = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just Closure.Info {info}
      | NameSymbol.toSymbol name == symbolName ->
        pure $ fromMaybe Context.default' (Context.precedenceOf info)
    Just Closure.Info {mOpen = Just prefix} ->
      contextCase (prefix <> name)
    Just Closure.Info {} ->
      throw @"error" (UnknownSymbol name)
    Nothing ->
      contextCase name
  where
    contextCase name =
      case Context.lookup name ctx >>= extractInformation . Context.extractValue of
        Nothing -> throw @"error" (UnknownSymbol name)
        Just pr -> pure (fromMaybe Context.default' (Context.precedenceOf pr))

------------------------------------------------------------
-- binderDispatchWith function dispatch table
------------------------------------------------------------

primitive :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
primitive (Structure.toPrimitive -> Just prim) cont
  | Just Sexp.A {atomName} <- Sexp.atomFromT (prim ^. name) = do
    -- this is bad, however it's FINE, as we only have the primitive
    -- which no operation should disrupt.
    -- Namely if it's (:primitive Michelson.Foo)
    -- we add Michelson to the closure, which is wrong.
    -- However, since we just have the primitive at this point, we
    -- can't have any other symbol named Michelson in this scope, so
    -- it's not an actual issue.
    local @"closure" (Closure.insertGeneric (NameSymbol.hd atomName)) $ do
      prim <- cont (prim ^. name)
      pure $ Structure.fromPrimitive $ Structure.Primitive prim
primitive _ _ = error "malformed primitive"

lambda :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
lambda (Structure.toLambda -> Just lam) cont =
  local @"closure" (\c -> foldr Closure.insertGeneric c (nameStar (lam ^. args))) $
    mapMOf body cont lam
      >>= mapMOf args cont
      >>| Structure.fromLambda
lambda _ _ = error "malformed lambda"

letType :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
letType (Structure.toLetType -> Just let') cont = do
  let bindings = nameStar (let' ^. args)
      consturctors = nameGather (let' ^. body)
      closureUpdate cnt =
        foldr Closure.insertGeneric cnt (bindings <> consturctors)
  local @"closure" closureUpdate $
    mapMOf body cont let'
      >>= mapMOf nameAndSig cont
      >>= mapMOf rest cont
      >>| Structure.fromLetType
letType _ _ = error "malformed let-type"

type' :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
type' (Structure.toType -> Just type') cont =
  let grabBindings = nameStar (type' ^. args)
   in local @"closure" (\cnt -> foldr Closure.insertGeneric cnt grabBindings) $
        mapMOf body cont type'
          >>= mapMOf nameAndSig cont
          >>| Structure.fromType
type' s _ = panic $ "malformed type: " <> show s

-- | @openIn@ opens @mod@, adding the contents to the closure of
-- @body@. Note that we first =resolve= what mod is by calling the
-- continuation, @cont@, in case any transformations want to change
-- what the @mod@ is.
openIn ::
  (ErrS f, HasClosure f) => SexpContext -> Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
openIn ctx (Structure.toOpenIn -> Just open) cont = do
  -- Fully run what we need to on mod
  newMod <- cont (open ^. name)
  -- Now let us open up the box
  case Sexp.atomFromT newMod of
    Just Sexp.A {atomName} ->
      case ctx Context.!? atomName >>| Context.extractValue of
        Just (Context.Record record) ->
          let NameSpace.List {publicL} = NameSpace.toList (record ^. Context.contents)
              --
              newSymbs = Juvix.Library.fst <$> publicL
              --
              addSymbolInfo symbol =
                Closure.insert symbol (Closure.Info Nothing [] (Just atomName))
           in --
              local @"closure" (\cnt -> foldr addSymbolInfo cnt newSymbs) $
                mapMOf body cont open
                  >>| set name newMod
                  >>| Structure.fromOpenIn
        _ ->
          throw @"error" (CantResolve [newMod])
    _ ->
      throw @"error" (CantResolve [newMod])
openIn _ _ _ = error "malformed open-in"

-- | @lambdaCase@ we encounter a @:lambda-case@ at the start of every
-- Definition in the context. This ensures the arguments are properly
-- bound for the inner computation.
lambdaCase :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
lambdaCase (Structure.toLambdaCase -> Just (Structure.LambdaCase args)) cont =
  traverse (`matchMany` cont) args
    >>| Structure.LambdaCase
    >>| Structure.fromLambdaCase
lambdaCase _ _ = error "malformed lambda case"

letMatch :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
letMatch (Structure.toLetMatch -> Just let') cont
  | Just nameSymb <- eleToSymbol (let' ^. name) =
    local @"closure" (Closure.insertGeneric nameSymb) $
      mapMOf args (traverse (`matchMany` cont)) let'
        >>= mapMOf body cont
        >>| Structure.fromLetMatch
letMatch _ _ = error "malformed let-match"

-- is the structure the same? seems like there is an error here

-- | @handler@ follows the exact same logic as @let-match@
handler :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
handler sexp cont =
  letMatch (Sexp.Cons (Sexp.atom Structure.nameLetModule) (Sexp.cdr sexp)) cont
    >>| Sexp.Cons (Sexp.atom Structure.nameLetHandler) . Sexp.cdr

-- | @case'@ is similar to @lambdaCase@ except that it has a term it's
-- matching on that it must first change without having an extra
-- binders around it
case' :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
case' (Structure.toCase -> Just case') cont =
  mapMOf on cont case'
    >>= mapMOf implications (traverse (`match` cont))
    >>| Structure.fromCase
case' _ _ = error "malformed case"

-- This works as we should only do a declaration after the function
-- locally, so if it gets overwritten its' not a big deal

-- | @declaim@ takes a declaration and adds the declaration information
-- to the context
declaim :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
declaim (Structure.toDeclaim -> Just declaim) cont
  | Just (name, information) <- declaration (declaim ^. claim) =
    local @"closure" (Closure.insert name information) $
      -- safe to do dec here, as if we modify the declaration it
      -- would be fine to do it after, as all a pass would do is to
      -- make it a namesymbol, meaning it wouldn't work as is ☹
      mapMOf claim cont declaim
        >>= mapMOf body cont
        >>| Structure.fromDeclaim
declaim _ _ = error "malformed declaim"

------------------------------------------------------------
-- Helpers for the various Search and Closure dispatch
------------------------------------------------------------

-- | @matchMany@ deals with a @((binding-1 … binding-n) body) term, and
-- proper continues the transformation on the body, and the bindings
-- after making sure to register that they are indeed bound terms
matchMany ::
  HasClosure m => Structure.ArgBody -> (Sexp.T -> m Sexp.T) -> m Structure.ArgBody
matchMany = matchGen nameStar

-- | @match@ deals with a @(bindings body)@ term coming down, see
-- @matchMany@ for more details
match ::
  HasClosure m => Structure.DeconBody -> (Sexp.T -> m Sexp.T) -> m Structure.DeconBody
match arg f = matchGen nameStarSingle (fromDecon arg) f >>| toDecon
  where
    fromDecon arg =
      case Structure.toArgBody (Structure.fromDeconBody arg) of
        Nothing -> error "deconToArgBody failed... how?"
        Just v -> v
    toDecon arg =
      case Structure.toDeconBody (Structure.fromArgBody arg) of
        Nothing -> error "argBodyToDecon failed... how?"
        Just v -> v

-- | @matchGen@ is a generic/general version of match and matchMany as
-- the form that comes in may be a list of binders or a single term
-- being bound.
matchGen ::
  (HasClosure m, Foldable t) =>
  (Sexp.T -> t Symbol) ->
  Structure.ArgBody ->
  (Sexp.T -> m Sexp.T) ->
  m Structure.ArgBody
matchGen nameStarFunc argbody cont =
  -- Important we must do this first!
  local @"closure" (\cnt -> foldr Closure.insertGeneric cnt grabBindings) $
    -- THIS MUST happen in the local, as we don't want to have a pass
    -- confuse the variables here as something else... imagine if we
    -- are doing a pass which resolves symbols, then we'd try to
    -- resolve the variables we bind. However for constructors and what
    -- not they need to be ran through this pass
    mapMOf args cont argbody
      >>= mapMOf body cont
  where
    grabBindings = nameStarFunc (argbody ^. args)

-- | @nameStarSingle@ like @nameStar@ but we are matching on a single
-- element
nameStarSingle :: Sexp.T -> [Symbol]
nameStarSingle = nameStar . (\x -> Sexp.list [x])

-- | @nameStar@ grabs names recursively
nameStar :: Sexp.T -> [Symbol]
nameStar ((_caar Sexp.:> cadr) Sexp.:> cdr) =
  -- we ignore the _caar as it's a cosntructor!
  nameStar cadr <> nameStar cdr
nameStar (x Sexp.:> xs)
  | Just symb <- eleToSymbol x =
    symb : nameStar xs
  | otherwise =
    -- the car is not a cons or an atom, thus a number, we should
    -- ignore it
    nameStar xs
nameStar Sexp.Atom {} = []
nameStar Sexp.Nil = []

-- Sexp.parse "((Cons (:arrow (:infix -> Int Int))) (Nil))" >>| nameGather

-- | @nameGather@ takes an adt sexp and extracts the constructors from it
nameGather :: Sexp.T -> [Symbol]
nameGather ((caar Sexp.:> _cdar) Sexp.:> cdr)
  | Just symb <- eleToSymbol caar,
    symb /= ":" || symb /= ":record-d" =
    symb : nameGather cdr
nameGather (_ Sexp.:> cdr) = nameGather cdr
nameGather _ = []

------------------------------------------------------------
-- Helpers for declaim
------------------------------------------------------------

-- | @declaration@ takes a declaration and tries to get the information
-- along with the name from it.
-- - Note :: we can only get symbol declarations to update, as we rely
--   on closure semantics whicich only work on symbols unfortunately.
declaration :: Sexp.T -> Maybe (Symbol, Closure.Information)
declaration (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    let func =
          if
              | Sexp.isAtomNamed inf "infix" ->
                Context.NonAssoc
              | Sexp.isAtomNamed inf "infixl" ->
                Context.Left
              | Sexp.isAtomNamed inf "infixr" ->
                Context.Right
              | otherwise -> error "malformed declaration"
     in Just
          ( atomName,
            Closure.Info
              Nothing
              [Context.Prec $ Context.Pred func (fromInteger atomNum)]
              Nothing
          )
declaration _ = Nothing

------------------------------------------------------------
-- Move to Sexp library
------------------------------------------------------------

eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
