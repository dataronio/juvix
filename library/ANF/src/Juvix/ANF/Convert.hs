module Juvix.ANF.Convert where

import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import Juvix.ANF.IR

convert :: Has2Closures m n => HandlerContext m n  -> HandlerContext m n
convert sexp = Sexp.foldPred sexp isEffectful conv
  where
    conv atom cdr
      | Sexp.isAtomNamed atom "handler" = fmap (Sexp.Cons cdr) (convertHandler atom)
      | otherwise                       = error "something weird happened"

-- at this point, continuations are only popped outta the stack
convHandler :: Has2Closures m n => HandlerContext m n  -> HandlerContext m n
convHandler prog = do
  h <- get "pure"
  k <- get "effectful"
  return $ Sexp.list
    [ Sexp.atom ":let-match",
      undefined,
      matchCases prog,
      forward prog
    ]
  where
    matchCases prog = Sexp.list [ Sexp.atom ":lambda",
                                  Sexp.list [ Sexp.atom ":as",
                                              Sexp.atom (hash prog)]

-- vmap relays continuations to outer handlers if necessary
-- vmap ignores operations for now
-- TODO add operations to vmap
vmap f val prog = do
  h <- get "pure"
  return $ f val (\x -> k x) prog
