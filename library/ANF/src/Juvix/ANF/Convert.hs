module Juvix.ANF.Convert where

import Juvix.Library
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.EffectHandlerHelpers as Str
import qualified Juvix.Sexp.Structure.Transition as Str
import qualified Juvix.Closure as Closure
import qualified Juvix.ANF.AnonymClosure as Stack
import qualified Juvix.Contextify.Environment as Env

convert :: Sexp.T -> Sexp.T
convert sexp = Sexp.foldPred sexp isEffectful conv
  where
    convVia = convertVia . toVia
    conDo = convertDo . toDoDeep
    conDoOp = convertDoOp . toDoOp
    conDoPure = convertDoPure . toDoPure
    conv atom cdr
      | Str.isDo atom     = Sexp.Cons (conDo atom) (convert cdr)
      | Str.isVia atom    = Sexp.Cons (conVia atom) (convert cdr)
      | Str.isDoOp atom   = Sexp.Cons (conDoOp atom) (convert cdr)
      | Str.isDoPure atom = Sexp.Cons (conDoPure atom) (convert cdr)

      -- if no effectful code, no CPS transformation
      | otherwise         = Sexp.Cons atom cdr

convertDo (Str.DoDeep {doStatements}) = undefined

convertVia (Str.Via dos@(Str.Do {..}) (Str.LetHandler {..})) =
  app' (convert dos) [handRet, handLet]
  where
    handRet =
      -- \k. k [[V]]
      lam (var "k")
      $ app (var "k") (convert letRetBody)

    handLet = letMatch handlerName (matchCases <$> letHandlerOp) skip

    matchCases (Str.LetOp {..}) = Str.fromArgBody $ Str.ArgBody undefined undefined


    -- reshapes operation to match outer handler
    mForward = lam (Sexp.atom "k1")
               $ lam (Sexp.atom "h1")
               vmap (lam () (app (var "k") (pair (var "p") (lam (var "x") ())))) (var "y") (var "h1")

    -- vmap relays continuations to outer handlers if necessary
    vmap f op k
      | isPair op =
        case op of
          _ Sexp.:> label Sexp.:> value ->
            -- f value (\x k. k (label x)) k
            app' f [value, lam' [var "x", var "k"] (app (var "k") (app label (var "x"))), k]
          _ -> error "shouldn't happen"

convertDoPure (Str.DoPure {..}) =
  -- \k. k [[V]]
  lam (var "k") $ app (var "k") (convert doPureArg)

convertDoOp (Str.DoOp {..}) =
  -- \k.\h.h (l <[[v]], \x. k x h)
  lam' [var "k", var "h"]
  $ app (var "h")
  $ triple doOpName (convert doOpArgs)
  $ lam (var "x")
  $ app' (var "k") [var "x", var "h"]


isEffectful :: Sexp.T -> Bool
isEffectful sexp = foldr (||) False ([ Str.isDo sexp
                                     , Str.isVia sexp
                                     , Str.isDoOp sexp
                                     , Str.isDoPure sexp
                                     ])

lam = Str.fromLam $ Str.Lam
app = Str.fromApp $ Str.App
letMatch = Str.fromLetMatch $ Str.LetMatch
pair = Str.fromPair $ Str.Pair
-- move to record
triple name value = Str.fromPair $ Str.Pair name (Str.fromPair $ Str.Pair value cont)
app' f args = Sexp.fold (\acc arg -> app acc arg) f args
lam' args body = undefined
var = Sexp.atom
skip = Sexp.nil
