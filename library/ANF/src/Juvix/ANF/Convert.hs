module Juvix.ANF.Convert where

import Juvix.Library
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.EffectHandlerHelpers as Str
import qualified Juvix.Sexp.Structure.Transition as Str
import qualified Juvix.Closure as Closure
import qualified Juvix.ANF.AnonymClosure as Stack
import qualified Juvix.Contextify.Environment as Env

type FXClosure m n = ( HasReader "pure" Closure.T m
                     , HasReader "effectful" Closure.T n)

data EffectContextI = FXContextI
  { pure :: Stack.T
  , effectful :: Stack.T
  , vars :: Closure.T
  }

data ErrorEff = NoEffect

type EffectContextA = ExceptT Env.ErrorS (State EffectContextI)

newtype EffectContext = FXContext { run :: EffectContextA }
  deriving (Functor, Applicative, Monad)
  deriving
    (HasReader "pure" Closure.T,
     HasSource "pure" Closure.T
    )
    via ReaderField "pure" EffectContextA
  deriving
    (HasReader "effectful" Closure.T,
     HasSource "effectful" Closure.T
    )
    via ReaderField "effectful" EffectContextA
  deriving
    (HasThrow "error" Env.ErrorS)
    via MonadError EffectContextA

nextPureCont = do
  p <- get@"pure"
  let cont = Stack.head p
  set @"pure" (Stack.tail p)
  return cont

nextEffectCont = do
  k <- get@"effectful"
  let cont = Stack.head p
  set @"effectful" (Stack.tail k)
  return cont

addPureCont cont =
  ps <- get@"pure"
  set @"pure" (Stack.cons (convert cont) ps)
  return ()

addEffectConts cont =
  ks <- get@"effectful"
  set @"effectful" (addToStack conts ks)
  return ()
  where
    addToStack conts ks =
      let ks' = convert <$> Sexp.toList conts
      in Stack.append ks' ks

convert sexp = Sexp.foldPred sexp isEffectful conv
  where
    convVia = convertVia . mapViaStr . toVia
    conv atom cdr
      | Str.isVia     = Sexp.Cons cdr <$> conVia atom
      | Str.isHandler = undefined
      | Str.isLetOp   = undefined
      | Str.isLetRet  = undefined
      | otherwise     = return sexp -- if no effectful code, no CPS transformation

convHandler prog = do
  return $ Sexp.list
    [ Sexp.atom Str.nameLetMatch,
      undefined,
      matchCases prog,
      forward prog
    ]
  where
    matchCases Str.LetOp  = undefined
    matchCases Str.LetRet = undefined

    forward prog = do
      h <- nextPCont
      k <- nextECont
      h' <- nextPCont
      k' <- nextECont
      return $ undefined

-- convertDo :: Has2Closures m n => HandlerContext m n -> HandlerContext m n
convertDo (Str.Do {doStatements})= do
   h <- nextPCont
   k <- nextECont
   return $ undefined -- l (V, \x -> k x h)


convertVia (Str.Via dos@(Str.Do {..}) lh@(Str.LetHandler {letHandlerOps, letHandlerRet})) =
  do
  add
  Str.App (convert dos) (convert lh)


convertLetRet (Str.LetRet {letRetBody}) = do
  k <- nextPCont
  return (Str.App k letRetBody)

-- vmap relays continuations to outer handlers if necessary
-- vmap :: Has2Closures m n => HandlerContext m n -> HandlerContext m n
vmap f val prog = do
  h <- get @"pure"
  return $ f val (\x -> k x) prog

isEffectful :: Sexp.T -> Bool
isEffectful sexp = foldr (||) False ([Str.isHandler, Str.isLetHandler, Str.isVia, Str.isLetRet, Str.isLetOp])
