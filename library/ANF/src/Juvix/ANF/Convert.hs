{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveAnyClass #-}

module Juvix.ANF.Convert where

import Juvix.Library hiding (fst, snd, head, tail, list)
import qualified System.IO.Unsafe as YOLO
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.EffectHandlerHelpers as Str
import qualified Juvix.Sexp.Structure as Str
import qualified Juvix.Sexp.Structure.CoreNamed as Str
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Data.Unique as Uni

-- | The CPS translation implemented here deals with higher-order continuations.
-- | We work with continuations at the meta-language level (Haskell) and
-- | source language (Juvix post-effects). For that, we convert the stack
-- | between static and dynamic contexts.
-- | Cryptic parts of the code are commented with a version that is more human-readable
-- | and it appears above and horizontally-aligned with the code it explains.

-- IO to generate new names that are likely unique
type CPSTrans a = IO a

-- Top-level function with implicit IO effect for new names
convertEffects :: Sexp.T -> Sexp.T
convertEffects sexp = Sexp.foldPred sexp (== Str.nameVia) conv
  where
    conv atom cdr
      -- IO to generate new unique names, no big deal
      | Just via_ <- Str.toVia sexp =
          YOLO.unsafeDupablePerformIO
                            -- starts with an empty stack
          $ convertVia via_ []

      -- if no effectful code, no CPS transformation
      -- all effect structures are removed
      | otherwise = removeEffectful sexp

-- Since there's no `via` to generate a stack of continuations,
-- transforming other constructs makes no sense.
-- TODO: Transform handlers into records for typechecking
removeEffectful :: Sexp.T -> Sexp.T
removeEffectful x = x

-- `convert` takes care of the actual translation
convert :: Sexp.T -> Stack -> CPSTrans Sexp.T
convert sexp stack
  | Just do_    <- Str.toDoDeep sexp = convertDo     do_    stack
  | Just via_   <- Str.toVia    sexp = convertVia    via_   stack
  | Just doOp   <- Str.toDoOp   sexp = convertDoOp   doOp   stack
  | Just doPure <- Str.toDoPure sexp = convertDoPure doPure stack
    -- TODO Lam, Let, LetMatch
    -- application under `via` must be CPS'ed as well
    -- type translations must be applied here as well once #788 is done
  | otherwise = pure sexp

-- the algorithm creates a stack of continuations that exists in both
-- static and dynamic contexts, ie higher-order continuations.
-- such stacks must be able to cross the static and dynamic lines
-- so we create operators reflect and reify.

-- a stack of continuations contains a list of `:defun` or anonymous lambdas
type Stack = [Sexp.T]

reify :: Stack -> Sexp.T
reify sexp = list Sexp.:> Sexp.list sexp

reflect :: Sexp.T -> Stack
reflect sexp = maybeToList $ Sexp.toList sexp
  where maybeToList (Just x) = x
        maybeToList Nothing = [sexp]

unique :: IO Sexp.T
unique = var . NameSymbol.fromString . show . Uni.hashUnique <$> Uni.newUnique

list = var ":list"
spair = var ":pair"

convertDo :: Str.DoDeep -> Stack -> CPSTrans Sexp.T
convertDo ((Str.DoDeep {..})) (k : ks) = convertDoBodyFull do_
  where convertDoBodyFull (Str.WithBinder {..}) = do
          x <- unique
          ks' <- unique
          -- other dos are chained by adding them to the continuation
          convDos <- convert (do' dos) (k : reflect ks')
                                        -- x is bound here
          convert doBodyFullBBody (lam' [x, ks'] convDos : ks)

        convertDoBodyFull (Str.NoBinder {..}) = do
          ks' <- unique
          -- other dos are chained by adding them to the continuation
          convDos <- convert (do' dos)  (k : reflect ks')
                                       -- no x to be bound here
          convert doBodyFullBody (lam ks' convDos : ks)

        (do_ : dos) = doDeepStatements
-- convertDo Nothing _ = pure skip

convertVia :: Str.Via -> Stack -> CPSTrans Sexp.T
convertVia ((Str.Via (Str.Handler {..}) dos@(Str.Do {..}))) ks = do
  hr <- handRet handlerRet
  hl <- handLet
  convert (Str.from dos) ( hr : hl : ks)

  where
    handRet (Str.LetRet {..}) = do
      x <- unique
      ks_ <- unique
                                             -- first continuation is discarded,
                                             -- since it was meant to operations
      converted <- convert letRetBody (reflect $ tail ks_)
      pure $ lam' [x, ks_] converted

    handLet = do
      z <- unique
      ks_ <- unique
      forward <- mForward ks_ z
      pure
        -- (fun handlerName (z) ->
        -- match z of
        --   opName args -> opBody
        --   _           -> forward
        -- ) z
        $ lam' [z, ks_]
        $ flip app z
        $ fun (var handlerName) z
        $ letMatch z (matchCases <$> handlerOps)
        $ forward

    -- converts operation into an match-argument for let-match
    matchCases :: Str.LetOp -> Str.ArgBody
    matchCases (Str.LetOp {..}) =
      Str.ArgBody (triple letOpName (var "p") (var "r")) letOpBody

    -- reshapes operation to match outer handler
    mForward ks y = do
      pairpr  <- unique
      ks_     <- unique
      k'      <- unique
      h'      <- unique
      ks'     <- unique
                      -- human-readable version of what's going on
                      -- λ〈p, s〉(k : ks). k 〈p, h' : k' : s〉 ks
      vmapped <- vmap (lam' [pairpr, ks_]
                       (app' (head ks_)
                        [pair (fst pairpr) (list Sexp.:> h' Sexp.:> k' Sexp.:> snd pairpr), tail ks_]))
                   y ks'
      pure
        $ lam (var "") (letMatch ks_ (argbody k' h' ks' vmapped) (skip))

    -- deconstructs a list
    argbody k' h' ks' body = [ Str.ArgBody (list Sexp.:> k' Sexp.:> h' Sexp.:> ks') body ]

    -- vmap relays continuations to outer handlers if necessary
    vmap :: Sexp.T -> Sexp.T -> Sexp.T -> CPSTrans Sexp.T
    vmap f op k
      | Str.isPair op =
        case op of
          _ Sexp.:> label Sexp.:> value -> do
            x <- unique
            k_ <- unique
                   --  human-readable version of what's going on
                   -- f value (λ x k. k (label x)) k
            pure $ app' f [value, lam' [x, k_] (app k_ (app label k_)), k]
          _ -> pure skip -- shouldn't happen
-- convertVia Nothing _ = pure skip

convertDoPure :: Str.DoPure -> Stack -> CPSTrans Sexp.T
convertDoPure ((Str.DoPure {..})) (k:ks) = do
  -- doesn't require a stack since it returns an expression
  converted <- convert doPureArg []
  pure $ app' k  [ converted , reify ks ]
-- convertDoPure Nothing _ = pure skip

convertDoOp :: Str.DoOp -> Stack -> CPSTrans Sexp.T
convertDoOp ((Str.DoOp {..})) (k:n:ks) = do
  -- doesn't require a stack since it returns an expression
  converted <-  convert doOpArgs []
  pure $ app' n [ triple doOpName converted (Sexp.list [n, k]), reify ks ]
-- convertDoOp Nothing _ = pure skip

-- combinators to create fragments of Juvix's Sexp
do' :: [Str.DoBodyFull] -> Sexp.T
do' x = Str.fromDoDeep $ Str.DoDeep x

lam :: Sexp.T -> Sexp.T -> Sexp.T
lam name x = Str.from $ Str.Lam name' x
  where (Just name') = Sexp.nameFromT name

app :: Sexp.T -> Sexp.T -> Sexp.T
app x y = Str.from $ Str.App x y

letMatch :: Sexp.T -> [Str.ArgBody] -> Sexp.T -> Sexp.T
letMatch x y z = Str.from $ Str.LetMatch x y z

fun :: Sexp.T -> Sexp.T -> Sexp.T -> Sexp.T
fun x y z = Str.from $ Str.Defun x y z

pair :: Sexp.T -> Sexp.T -> Sexp.T
pair x y = Str.from $ Str.Pair x y

-- multiple argument application
app' :: Sexp.T -> [Sexp.T] -> Sexp.T
app' f args = foldr (\arg acc -> app acc arg) f args

-- multiple argument lambda
lam' :: [Sexp.T] -> Sexp.T -> Sexp.T
lam' args body = foldr (\arg acc -> lam arg acc) body (reverse args)

var :: NameSymbol.T -> Sexp.T
var x = Sexp.atom x

skip :: Sexp.T
skip = Sexp.Nil

fst var_ = letMatch var_ argbody skip
  where argbody = [ Str.ArgBody  (spair Sexp.:> var "p" Sexp.:> var "q") (var "p") ]

snd var_ = letMatch var_ argbody skip
  where argbody = [ Str.ArgBody  (spair Sexp.:> var "p" Sexp.:> var "q") (var "q") ]

-- not sure how to pattern match on lists here
head var_ = letMatch var_ argbody skip
  where argbody = [ Str.ArgBody (list Sexp.:> var "x" Sexp.:> var "xs") (var "x") ]

tail var_ = letMatch var_ argbody skip
  where argbody = [ Str.ArgBody (list Sexp.:> var "x" Sexp.:> var "xs") (var "xs") ]

let_ :: Str.Binder -> Sexp.T -> Sexp.T
let_ b x= Str.from $ Str.Let b x

binder :: NameSymbol.T -> Sexp.T -> Str.Binder
binder name term = Str.Binder name (var ":omega") term

-- TODO: move to record
triple name value cont = Str.from $ Str.Pair name (Str.from $ Str.Pair value cont)
