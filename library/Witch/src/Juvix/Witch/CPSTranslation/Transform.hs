-- algorithm:
-- put all handlers into context
-- search for all vias
-- for all vias
--  - if current via contain any via, remove nested vias from main list
--  - create a stack for via's handler
--  - traverse over values and computations applying stacks as per rules
-- end for vias
-- remove all handlers from context
{-# LANGUAGE DeriveAnyClass #-}
-- algorithm:
-- put all handlers into context
-- search for all vias
-- for all vias
--  - if current via contain any via, remove nested vias from main list
--  - create a stack for via's handler
--  - traverse over values and computations applying stacks as per rules
-- end for vias
-- remove all handlers from context
{-# LANGUAGE DeriveFunctor #-}
-- algorithm:
-- put all handlers into context
-- search for all vias
-- for all vias
--  - if current via contain any via, remove nested vias from main list
--  - create a stack for via's handler
--  - traverse over values and computations applying stacks as per rules
-- end for vias
-- remove all handlers from context
{-# LANGUAGE RecordWildCards #-}

-- | The CPS translation implemented here deals with higher-order continuations.
-- | We work with continuations at the meta-language level (Haskell) and
-- | source language (Juvix post-effects). For that, we convert the stack
-- | between static and dynamic contexts.
-- | Cryptic parts of the code are commented with a version that is more human-readable
-- | and it appears above and horizontally-aligned with the code it explains.
module Juvix.Witch.CPSTranslation.Transform where

import qualified Data.Unique as Uni
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Precedence as Pred
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.CoreNamed as Core
import qualified Juvix.Sexp.Structure.Parsing as Str
import qualified Juvix.Sexp.Structure.Transition as Str
import qualified Juvix.Witch.CPSTranslation.SexpHelpers as Sexp

type ExpressionIO m = (MonadIO m, MonadFail m)

-- TODO make a Stack capability
type Stack = [Sexp.T]

type Stacked m a = m (Stack, a)

-- TODO make a Context capability
type ContextifiedProgram m ty sumRep = m (Context.T Sexp.T ty sumRep, [Sexp.T])

type ContextifiedSexp m ty sumRep = m (Context.T Sexp.T ty sumRep, Sexp.T)

type ContextifiedStack m ty sumRep = m (Context.T Sexp.T ty sumRep, Stack)

type CPSTrans m ty sumRep = m (Context.T Sexp.T ty sumRep, [Sexp.T], Stack)

-- initial return continuation
retId :: IO Sexp.T
retId = do
  x <- unique
  pure $ Sexp.lam' [x] (x)

-- initial op continuation
opId :: IO Sexp.T
opId = do
  z <- unique
  k <- unique
  pure $ Sexp.lam z (Sexp.app k z)

-- initial stack
initStk :: IO Stack
initStk = do
  k <- retId
  h <- opId
  pure [k, h]

-- make CPSTrans datatype
mkTrans ::
  ExpressionIO m =>
  Context.T Sexp.T ty sumRep ->
  [Sexp.T] ->
  Stack ->
  CPSTrans m ty sumRep
mkTrans ctx sexp stk = pure (ctx, sexp, stk)

-- make Staked datatype
mkStkd :: ExpressionIO m => Stack -> a -> Stacked m a
mkStkd stack a = pure (stack, a)

mkContextProg ::
  ExpressionIO m =>
  Context.T Sexp.T ty sumRep ->
  [Sexp.T] ->
  ContextifiedProgram m ty sumRep
mkContextProg ctx sexp = pure (ctx, sexp)

-- reify stacks
reify :: Stack -> Sexp.T
reify = Sexp.list

-- reflect stacks
reflect :: Sexp.T -> Stack
reflect = concat . maybeToList . Sexp.toList

-- generate unique names
unique :: IO Sexp.T
unique = Sexp.var . NameSymbol.fromString . show . Uni.hashUnique <$> Uni.newUnique

-- collect any construct by name
collect :: NameSymbol.T -> [Sexp.T] -> [Sexp.T]
collect name form =
  fmap (Sexp.findKey Sexp.car (Sexp.atom name) . toList) form >>= maybeToList
  where
    -- toList exists to add a list around
    -- the target structure in case it's an immediate one
    toList :: Sexp.T -> Sexp.T
    toList x = x Sexp.:> Sexp.Nil

-- collect all handlers
collectHandlers :: [Sexp.T] -> [Sexp.T]
collectHandlers = collect Str.nameHandler

-- collect all vias
-- if current via contains other vias, remove said vias from the list
collectVia :: [Sexp.T] -> [Sexp.T]
collectVia prog = fst $ foldr removeNestedVia ([], []) vias
  where
    vias :: [Sexp.T]
    vias = collect Str.nameVia prog

    removeNestedVia :: Sexp.T -> ([Sexp.T], [Sexp.T]) -> ([Sexp.T], [Sexp.T])
    removeNestedVia via_ (vias, removed)
      | via_ `isIn` removed = (vias, removed)
      | otherwise = (via_ : vias, removed ++ nestedVias via_)

    isIn :: Sexp.T -> [Sexp.T] -> Bool
    isIn sexp ss = foldr (\s acc -> s == sexp || acc) False ss

    nestedVias :: Sexp.T -> [Sexp.T]
    nestedVias sexp = collect Str.nameVia [sexp]

-- convert all vias
convertVias ::
  ExpressionIO m =>
  ContextifiedProgram m ty sumRep ->
  ContextifiedProgram m ty sumRep
convertVias cpstrans = do
  (ctx, sexp) <- cpstrans
  stk <- liftIO initStk
  (context, sexp, _) <- foldr convert (mkTrans ctx [] stk) sexp
  pure (context, sexp)

-- look for all handlers in form, and add them to context
addHandlersToContext ::
  ExpressionIO m =>
  Context.T Sexp.T ty sumRep ->
  [Sexp.T] ->
  ContextifiedProgram m ty sumRep
addHandlersToContext context form =
  let stk = collectHandlers form
      context' = foldr addToContext context stk
      newForm = removeHandlers form
   in pure (context', newForm)
  where
    addToContext handler context
      | Just (Str.Handler {..}) <- Str.toHandler handler = do
        Context.add (mkNameSpaceFrom handlerName) (mkDef handler) context
      | otherwise = context

-- make NameSpace.From
mkNameSpaceFrom :: Sexp.T -> NameSpace.From Symbol
mkNameSpaceFrom name
  | Just nm <- Sexp.nameFromT name = NameSpace.Pub (NameSymbol.toSym nm)
  | otherwise = NameSpace.Pub $ NameSymbol.toSym (NameSymbol.fromText "")

-- make Context.Definition
mkDef :: Sexp.T -> Context.Definition Sexp.T b c
mkDef form = Context.Def (Context.D Nothing Nothing form Pred.default')

-- delete Stack from Staked
deleteStack :: Stacked IO [Sexp.T] -> IO [Sexp.T]
deleteStack acc = do
  (_, sexp) <- acc
  pure sexp

-- remove handlers from program
removeHandlers :: [Sexp.T] -> [Sexp.T]
removeHandlers = foldr rmvHand []
  where
    rmvHand form acc
      | Just _ <- Str.toHandler form = acc
      | otherwise = form : acc

-- lookup name within context
construct :: Sexp.T -> Context.T Sexp.T ty sumRep -> Maybe Sexp.T
construct sexp ctx
  | Just var <- Core.toVar sexp =
    extractValue <$> Context.lookupCurrent (Core.varName var) ctx
  | otherwise = Just sexp
  where
    extractValue (NameSpace.Pub (Context.Def Context.D {..})) = defTerm
    extractValue (NameSpace.Priv (Context.Def Context.D {..})) = defTerm
    extractValue _ = Sexp.empty

-- `convert` takes care of the actual translation
convert :: ExpressionIO m => Sexp.T -> CPSTrans m ty sumRep -> CPSTrans m ty sumRep
convert sexp form = do
  (ctx, sexp1, stack) <- form
  conv ctx stack sexp1
  where
    conv ctx stack sexp1
      | Just via_ <- Str.toVia sexp = do
        (st, sexp2) <- convertVia via_ (mkStkd stack ctx)
        pure (ctx, sexp2 : sexp1, st)
      | Just (Str.Do {..}) <- Str.toDo sexp = do
        Just dos <- pure (construct doStatements ctx)
        (_, sexp2, st) <- convertDo (Str.Do dos) (mkStkd stack ctx)
        pure (ctx, sexp2 ++ sexp1, st)
      | Just doOp <- Str.toDoOp sexp = do
        (st, sexp2) <- convertDoOp doOp (mkStkd stack ctx)
        pure (ctx, sexp2 : sexp1, st)
      | Just doPure <- Str.toDoPure sexp = do
        (st, sexp2) <- convertDoPure doPure (mkStkd stack ctx)
        pure (ctx, sexp2 : sexp1, st)
      | Just lam_ <- Core.toLam sexp = do
        (st, sexp2) <- convertLam lam_ ctx
        pure (ctx, sexp2 : sexp1, st)
      | Just (Core.Let {Core.letBinder = Core.Binder {..}, Core.letBody = letBody}) <- Core.toLet sexp = do
        Just binder <- pure (construct binderTerm ctx)
        Just body <- pure (construct letBody ctx)
        (st, sexp2) <-
          convertLet
            (Core.Let (Core.Binder binderName binderUsage binder) body)
            (mkStkd stack ctx)
        pure (ctx, sexp2 : sexp1, st)
      | Just (Core.App {..}) <- Core.toApp sexp = do
        Just lambda <- pure (construct appFun ctx)
        Just args <- pure (construct appArg ctx)
        (st, sexp2) <- convertApp (Core.App lambda args) (mkStkd stack ctx)
        pure (ctx, sexp2 : sexp1, st)

      -- type translations must be applied here as well once #788 is done

      -- otherwise is reserved to values
      | otherwise = pure (ctx, sexp : sexp1, stack)

-- Similar to `convertDo`
convertLet ::
  ExpressionIO m =>
  Core.Let ->
  Stacked m (Context.T Sexp.T ty sumRep) ->
  m (Stack, Sexp.T)
convertLet (Core.Let {Core.letBinder = Core.Binder {..}, Core.letBody = letBody}) form = do
  (k : ks, ctx) <- form
  ks' <- liftIO unique
  (_, sexp : _, _) <- convert letBody (mkTrans ctx [] (k : reflect ks'))
  -- doBodyFullName is bound here
  stk <- pure (Sexp.lam' [Sexp.var binderName, ks'] sexp : ks)
  (_, sexp' : _, stk') <- convert (Sexp.cdr binderTerm) (mkTrans ctx [] stk)
  mkStkd stk' sexp'

-- Transforms applications to apply stack of continuations
convertApp ::
  ExpressionIO m =>
  Core.App ->
  Stacked m (Context.T Sexp.T ty sumRep) ->
  m (Stack, Sexp.T)
convertApp (Core.App {..}) stkd = do
  (stk, context) <- stkd
  (_, lambda : _, _) <- convert appFun $ mkTrans context [] []
  (_, args : _, st') <- convert appArg $ mkTrans context [] stk
  pure (st', Sexp.app' lambda [args, reify st'])

-- Transforms lambdas to take a stack of continuations
convertLam ::
  ExpressionIO m =>
  Core.Lam ->
  Context.T Sexp.T ty sumRep ->
  m (Stack, Sexp.T)
convertLam (Core.Lam {..}) context = do
  ks <- liftIO $ unique
  (_, sexp : _, st) <- convert lamBody $ mkTrans context [] (reflect ks)
  pure (st, Sexp.lam' [Sexp.var lamName, ks] (Sexp.app sexp (Sexp.list st)))

-- Translates a list of dos
-- do <x> <- <M> ==> λκ:κs. convert M ((λx ks. convert nextDo (κ : reflect ks)) :: κs)
convertDo ::
  ExpressionIO m =>
  Str.Do ->
  Stacked m (Context.T Sexp.T ty sumRep) ->
  CPSTrans m ty sumRep
convertDo do'@(Str.Do {..}) form = convertDoBodyFull do_ form
  where
    convertDoBodyFull ::
      ExpressionIO m =>
      Str.DoBodyFull ->
      Stacked m (Context.T Sexp.T ty sumRep) ->
      CPSTrans m ty sumRep
    convertDoBodyFull (Str.WithBinder {..}) form = do
      (k : ks, ctx) <- form
      ks' <- liftIO unique
      -- discard returned stack because it's always empty
      (_, sexp : _, _) <- convert (Sexp.do' dos) (mkTrans ctx [] (k : reflect ks'))
      -- doBodyFullName is bound here
      stk <- pure (Sexp.lam' [Sexp.var doBodyFullName, ks'] sexp : ks)
      convert (Sexp.cdr doBodyFullBBody) (mkTrans ctx [] stk)
    convertDoBodyFull (Str.NoBinder {..}) form = do
      (k : ks, ctx) <- form
      ks' <- liftIO unique
      -- discard returned stack because it's always empty
      (_, sexp, _) <- convert (Sexp.do' dos) (mkTrans ctx [] (k : reflect ks'))
      -- no variables to be bound here
      stk <- pure (Sexp.lam ks' (Sexp.list sexp) : ks)
      convert (Sexp.cdr doBodyFullBody) (mkTrans ctx [] stk)

    -- other dos are chained by adding them to the continuation
    Just (Str.DoDeep (do_ : dos)) = Str.toDoDeep (Str.fromDo do')

-- <prog> via <handler> ==> λκs. convert prog (convert handler [])
convertVia ::
  ExpressionIO m =>
  Str.Via ->
  Stacked m (Context.T Sexp.T ty sumRep) ->
  Stacked m Sexp.T
convertVia (Str.Via {..}) form = do
  (ks, ctx) <- form
  Just handler <- pure (construct viaHandler ctx)
  Just program <- pure (construct viaProgram ctx)

  (_, _, stk) <- convert handler $ mkTrans ctx [] ks
  (_, sexp : _, stk') <- convert program $ mkTrans ctx [] (stk ++ ks)

  pure (stk', sexp)

-- just triggers translation of return and operations
convertHandler :: ExpressionIO m => Sexp.T -> Stacked m (Context.T Sexp.T ty sumRep) -> m Stack
convertHandler sexp acc
  | Just hand <- Str.toHandler sexp = do
    convertHandler' hand acc
  | otherwise = do
    (stack, _) <- acc
    pure stack
  where
    convertHandler' ::
      ExpressionIO m =>
      Str.Handler ->
      Stacked m (Context.T Sexp.T ty sumRep) ->
      m Stack
    convertHandler' (Str.Handler {..}) acc = do
      (_, ctx) <- acc
      ret <- convertHandRet handlerRet ctx
      ops <- convertHandOp handlerOps ctx
      pure [ret, ops]

-- for (pure x -> N) ==> λx ks.let (h :: ks') = ks in (convert N) (reflect ks')
convertHandRet :: ExpressionIO m => Str.LetRet -> Context.T Sexp.T ty sumRep -> m Sexp.T
convertHandRet (Str.LetRet {..}) ctx = do
  ks_ <- liftIO $ unique
  Just program <- pure (construct letRetBody ctx)
  -- first continuation is discarded,
  -- since it was meant to pure/return
  (_, sexp : _, _) <- convert program (mkTrans ctx [] (reflect $ Sexp.cdr ks_))
  pure $ Sexp.lam' [Sexp.unlist letRetArg, ks_] (Sexp.app ks_ sexp)

-- for each (op args k -> N) ==> λz ks.case z {op〈p, s〉-> let r = reversion s in (convert N) reflect ks)
convertHandOp :: MonadIO m => [Str.LetOp] -> Context.T Sexp.T ty sumRep -> m Sexp.T
convertHandOp handlerOps ctx = do
  z <- liftIO $ unique
  ks_ <- liftIO $ unique
  args <- liftIO $ sequenceA $ (matchCases (reflect ks_) ctx) <$> handlerOps
  pure $
    Sexp.lam' [z, ks_] $
      Sexp.letMatch z args $
        Sexp.empty
  where
    -- converts operation into an match-argument for let-match
    matchCases :: Stack -> Context.T Sexp.T ty sumRep -> Str.LetOp -> IO Str.ArgBody
    matchCases stk ctx (Str.LetOp {..}) = do
      k_ <- liftIO $ unique
      Just program <- pure (construct letOpBody ctx)
      -- stack `stk` has been reflected already
      (_, body : _, _) <- convert program $ mkTrans ctx [] stk

      -- last argument is always the continuation
      Just k <- pure (Sexp.nameFromT . Sexp.last $ letOpArgs)
      args <- pure (Sexp.init letOpArgs)
      resumption <- liftIO $ reversion k_
      pure $ Str.ArgBody (Sexp.triple letOpName args k_) (Sexp.let_ (Sexp.binder k resumption) body)

    -- reversion is tricky !!
    -- the idea is to get a list of continuations we have passed to the labels
    -- and rebind it the last argument of an operation, k
    -- while transforming said list into a continuation that applies the value to the
    -- first continuation, and takes an x and a list of continuations
    -- in other words:
    -- let k = reversion [sn, ..., s1] ==> let k = \x ks -> s1 x (s2 :: ... :: sn :: ks)
    reversion :: Sexp.T -> IO Sexp.T
    reversion name = do
      x <- unique
      ks <- unique
      v <- unique
      vs <- unique
      pure $
        Sexp.letMatch
          name
          [ Str.ArgBody
              (v Sexp.:> vs)
              (Sexp.lam' [x, ks] $ Sexp.app' (v) [x, Sexp.list [vs, ks]])
          ]
          Sexp.empty

-- pure <value> ==>  λκ:κs. κ (convert value) (reify κs)
convertDoPure ::
  ExpressionIO m =>
  Str.DoPure ->
  Stacked m (Context.T Sexp.T ty sumRep) ->
  Stacked m Sexp.T
convertDoPure ((Str.DoPure {..})) form = do
  (k : ks, ctx) <- form
  Just program <- pure (construct doPureArg ctx)
  (_, sexp : _, st) <- convert program $ mkTrans ctx [] ks
  pure (st, (Sexp.app' k [sexp, reify st]))

-- <op> <value> ==> λκ:η:κs. η (〈op convert value, η :: κ :: []〉) (reify κs)
convertDoOp ::
  ExpressionIO m =>
  Str.DoOp ->
  Stacked m (Context.T Sexp.T ty sumRep) ->
  Stacked m Sexp.T
convertDoOp ((Str.DoOp {..})) form = do
  (k : n : ks, ctx) <- form
  Just program <- pure (construct doOpArgs ctx)
  (_, sexp : _, st) <- convert program $ mkTrans ctx [] [n, k]
  pure (st, Sexp.app' n [Sexp.triple doOpName sexp (Sexp.list [n, k]), reify ks])
