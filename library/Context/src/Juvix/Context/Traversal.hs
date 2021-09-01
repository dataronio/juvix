-- | Traversals serves as generic Context Traversal modules.
module Juvix.Context.Traversal where

import Control.Lens hiding ((|>))
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import Juvix.Context.Types
import Juvix.Library hiding (Sum, modify, toList)
import qualified Juvix.Library.HashMap as HashMap

newtype ExtraOptions = Extra {name :: NameSpace.From Symbol} deriving (Show)

------------------------------------------------------------
-- Traversal Function Transformers
------------------------------------------------------------
-- The ContextForm Transformers are a generic Transformer Interface for
-- maps that run over the context and may change the Definitions in
-- some way. Adding new ones, changing the type of Definitions to
-- commit back etc. Note that only Definitions and TypeDeclarations can
-- be removed/changed.

class ToContextFormGeneral contextForm where
  toContextForm ::
    Monad m => contextForm m term ty sumRep -> ContextFormGeneral m term ty sumRep

class
  ToContextFormGeneral contextTo =>
  FromContextFormSingle (contextFrom :: ((* -> *) -> * -> *)) contextTo
    | contextFrom -> contextTo
  where
  promote :: Monad m => contextFrom m term -> contextTo m term term term
  toContextSingle :: Monad m => contextFrom m term -> ContextFormGeneral m term term term
  toContextSingle = toContextForm . promote

-- | @ContextFormsGeneral@ is the most general form of the
-- @ContextForms@ in that it can commit back many definitions and
-- change the current form.
data ContextFormGeneral m term ty sumRep = CtxFormGeneral
  { sumGeneral :: sumRep -> T term ty sumRep -> ExtraOptions -> m (Additional sumRep term ty sumRep),
    termGeneral :: term -> T term ty sumRep -> ExtraOptions -> m (Additional term term ty sumRep),
    tyGeneral :: ty -> T term ty sumRep -> ExtraOptions -> m (Additional ty term ty sumRep)
  }
  deriving (Show)

data Additional t term ty sumRep = Additional
  { formPutBack :: Maybe t,
    extra :: [(NameSpace.From Symbol, Definition term ty sumRep)]
  }
  deriving (Show)

-- | @ContextForms@ is the simplest of the @ContextForms@, namely in
-- that it simply allows changing the underlying structure
data ContextForms m term ty sumRep = CtxForm
  { sumF :: sumRep -> T term ty sumRep -> m sumRep,
    termF :: term -> T term ty sumRep -> m term,
    tyF :: ty -> T term ty sumRep -> m ty
  }
  deriving (Show)

-- | @ContextSingle@ is a single function variant of
-- @ContextForms@
newtype ContextSingle m rep
  = CtxSingle (rep -> T rep rep rep -> m rep)
  deriving (Show)

-- Note we don't have a general ContextChange, as the term and ty are
-- ran on different parts to make it generic, thus only the sum
-- representation can safely change the definition

-- | @ContextChangeSumRep@ is a transformation that changes the sum
-- representation of a form into a new definition. If the resulting
-- function is Nothing then we don't change the definition
newtype ContextChangeSumRep m rep
  = CtxChangeSumRep
      ( rep ->
        T rep rep rep ->
        ExtraOptions ->
        m (Maybe (NameSpace.From Symbol, Definition rep rep rep))
      )
  deriving (Show)

instance ToContextFormGeneral ContextFormGeneral where
  toContextForm form = form

instance ToContextFormGeneral ContextForms where
  toContextForm CtxForm {sumF, termF, tyF} =
    CtxFormGeneral
      { sumGeneral = transform sumF,
        termGeneral = transform termF,
        tyGeneral = transform tyF
      }
    where
      transform f x y _extra = f x y >>| \res -> Additional (Just res) []

-- Due to generic issues we can't trasnform @ContextSingle@ into a
-- @ToContextFormGeneral@ interface.

instance FromContextFormSingle ContextSingle ContextForms where
  promote (CtxSingle f) = CtxForm f f f

instance FromContextFormSingle ContextChangeSumRep ContextFormGeneral where
  promote (CtxChangeSumRep f) =
    CtxFormGeneral newFunction newFunction newFunction
    where
      newFunction form ctx extra =
        f form ctx extra
          >>| \case
            Just (sym, def) -> Additional Nothing [(sym, def)]
            Nothing -> Additional (Just form) []

maybeAdditionToAddition ::
  Maybe (Additional t term ty sumRep) -> Additional t term ty sumRep
maybeAdditionToAddition (Just adds) = adds
maybeAdditionToAddition Nothing = Additional Nothing []

mapDef ::
  (a -> t) -> Additional a term ty sumRep -> Additional t term ty sumRep
mapDef f (Additional term extraDefs) = Additional (fmap f term) extraDefs

------------------------------------------------------------
-- Global Traversals
------------------------------------------------------------

-- | @mapWithContextPure@ is just @mapWithContext@ but is pure. Since
-- the function we take in is of type @ContextForms@ we must wrap our
-- functions in the identity monad. See @mapWithContext@ for more
-- information. We don't have to mutate, but often it's more useful
-- that way for various reasons. This pure version is very useful in a
-- simple pass through of the forms.
mapWithContextPure ::
  T term ty sumRep -> ContextForms Identity term ty sumRep -> T term ty sumRep
mapWithContextPure t f = runIdentity (mapWithContext t f)

-- | @overTopLevelMap@ runs a function, @f@ over a Context that is
-- switching between the namespaces of the @topLevelMap@ before finally
-- switching back.
overTopLevelMap ::
  Monad m => T term ty sum -> (T term ty sum -> m (T term ty sum)) -> m (T term ty sum)
overTopLevelMap t@T {topLevelMap, currentName} f = do
  ctx <- foldM switchAndUpdate t tops
  let Just finalCtx = Context.inNameSpace (Context.addTopName currentName) ctx
  pure finalCtx
  where
    tops = fmap (Context.addTopNameToSngle . pure) (HashMap.keys topLevelMap)
    switchAndUpdate ctx name =
      let Just t = Context.inNameSpace name ctx
       in f t

-- | @mapWithContext@ starts at the top of the context and applies the
-- given function f to all part of the data type it can. Note that
-- records are treated as if they are part of the module above it if
-- there is a signature!  See @mapCurrentContext@ for more information
-- about specifics, as we just dispatch to that for the real changes
mapWithContext ::
  (ToContextFormGeneral form, Monad m) =>
  T term ty sumRep ->
  form m term ty sumRep ->
  m (T term ty sumRep)
mapWithContext t f =
  -- If we map over every current module in the top level map, we map
  -- across the entire context!
  overTopLevelMap t (mapCurrentContext f)

-- | @mapWithName@ maps through the context definitions, supplying the
-- current name of the item given
mapWithName ::
  Monad m =>
  T tm ty sum ->
  (Definition tm ty sum -> NameSpace.From Symbol -> T tm ty sum -> m (Definition tm ty sum)) ->
  m (T tm ty sum)
mapWithName t f = overTopLevelMap t (mapWithCurrentName f)

-- | @mapSumWithName@ maps a function over the sum constructors of the context
mapSumWithName ::
  Monad m =>
  T term ty sumRep ->
  (SumT term ty -> Symbol -> T term ty sumRep -> m (Definition term ty sumRep)) ->
  m (T term ty sumRep)
mapSumWithName ctx f =
  mapWithName ctx fOnSum
  where
    fOnSum (SumCon s) name ctx =
      f s (NameSpace.extractValue name) ctx
    fOnSum def _ _ = pure def

-- | @mapCurrentContext@ maps f over the entire context, this function
-- calls the function handed to it over the context. The function runs
-- over the current form and the context, so any dependencies may be
-- resolved and proper stored away.
mapCurrentContext ::
  (ToContextFormGeneral form, Monad m) =>
  form m term ty sumRep ->
  T term ty sumRep ->
  m (T term ty sumRep)
mapCurrentContext transformers ctx =
  mapWithCurrentNameAddition dispatch ctx
  where
    f = toContextForm transformers
    -- we don't actually use the name of the current item at all
    -- so just ignore it
    dispatch form name ctx =
      -- Used for the def and sumcon case
      -- Just run over the two parts that change
      let callTyF typ =
            tyGeneral f typ ctx (Extra name)
          defCase d@D {defTerm, defMTy} = do
            Additional newTerm extraDefs <- termGeneral f defTerm ctx (Extra name)
            Additional defMTy extraDefs' <-
              traverse callTyF defMTy >>| maybeAdditionToAddition
            pure $
              Additional
                (fmap (\term -> d {defTerm = term, defMTy}) newTerm)
                (extraDefs <> extraDefs')
       in case form of
            Record r -> do
              -- call the signature change
              Additional newTy extraDefs <-
                fmap maybeAdditionToAddition (traverse callTyF (recordMTy r))
              -- setup the record where we recurse down it
              let recordToRecurseOn = r {recordMTy = newTy}
              pure $
                Additional
                  (Just (Record recordToRecurseOn))
                  extraDefs
            SumCon (Sum def name') ->
              traverse defCase def
                >>| maybeAdditionToAddition
                >>| \(Additional mNewDef extraDefs) ->
                  Additional (Just (SumCon (Sum mNewDef name'))) extraDefs
            Unknown mUnknown ->
              traverse callTyF mUnknown
                >>| maybeAdditionToAddition
                >>| \(Additional mUnknown extraDefs) ->
                  Additional (Just (Unknown mUnknown)) extraDefs
            Def definition'' -> defCase definition'' >>| mapDef Def
            TypeDeclar type' -> sumGeneral f type' ctx (Extra name) >>| mapDef TypeDeclar
            Information info -> pure (Additional (Just (Information info)) [])
            CurrentNameSpace -> pure (Additional (Just CurrentNameSpace) [])

mapWithCurrentName ::
  Monad m =>
  (Definition tm ty sp -> NameSpace.From Symbol -> T tm ty sp -> m (Definition tm ty sp)) ->
  T tm ty sp ->
  m (T tm ty sp)
mapWithCurrentName f =
  mapWithCurrentNameAddition liftF
  where
    liftF def name t = f def name t >>| (`Additional` []) . Just

mapWithCurrentNameAddition ::
  Monad m =>
  ( Definition tm ty sp ->
    NameSpace.From Symbol ->
    T tm ty sp ->
    m (Additional (Definition tm ty sp) tm ty sp)
  ) ->
  T tm ty sp ->
  m (T tm ty sp)
mapWithCurrentNameAddition f ctx@T {currentNameSpace, currentName} =
  foldM dispatch ctx names
  where
    names =
      NameSpace.toList1FSymb (currentNameSpace ^. contents)
    isRecord (Just (Record _)) = True
    isRecord _ = False
    dispatch ctx (name, form) = do
      -- we want to recurse on records handed back
      let recurseOnName ctx =
            case Context.inNameSpace (pure (NameSpace.extractValue name)) ctx of
              Just newCtx -> do
                ctx' <- mapWithCurrentNameAddition f newCtx
                let Just finalCtx =
                      Context.inNameSpace (Context.addTopName currentName) ctx'
                pure finalCtx
              Nothing ->
                pure ctx
      -- tuple return is so we don't have to repeat work in recurseOnName
      (updatedCtx, defMaybeRecord) <- do
        Additional mDef extraDefs <- f form name ctx
        let ctxWithName =
              case mDef of
                Nothing -> ctx
                Just def -> Context.add name def ctx
        newCtxAfterRecurse <- foldM dispatch ctxWithName extraDefs
        pure (newCtxAfterRecurse, mDef)
      if
          | isRecord defMaybeRecord -> recurseOnName updatedCtx
          | otherwise -> pure updatedCtx
