module Juvix.ANF.Transform where

import Juvix.Library
import qualified Juvix.Context as Ctx
import qualified Juvix.Closure.Abstract as Closure
import qualified Juvix.Sexp as Exp
import qualified Juvix.Sexp.Structure as Exp
import qualified Juvix.Library.NameSymbol as NameSymbol

-- adds to context and returns name
newSymbol :: NameSymbol.Name a => Closure.T Exp.T -> Closure.T a
newSymbol clo = (flip Closure.insertHash) exp

makeVars = Closure.T Exp.T -> Closure.T Exp.T
makeVars exp clo = Exp.fold exp (discriminate . checkAndTransform)
 where
   discriminate = undefined
   inToLet exp =
     let name = Exp.atom (newSymbol exp)
         args = Str.fromArgBody (Str.ArgBody exp name)
         body = name
     in (Str.FromLet (Str.LetMatch name args), clo)

transform :: Closure.T Exp.T -> Closure.T Exp.T
transform exp clo = Exp.foldPred exp isNonAtom discriminate
 where
   discriminate (Str.LetMatch name arg body) = undefined
   discriminate (Str.Tuple name _)           = undefined
   discriminate (Str.Lambda name args body)  = undefined
   discriminate (Str.Case name args body)    = undefined
   discriminate (Str.Arrow name args body)   = undefined
   discriminate (Str.App args fun)           = undefined
   discriminate (Str.ArgBody args fun)       = undefined

transLetMatch :: Closure.T Exp.T -> Closure.T Exp.T
transLetMatch exp =
  let _let = Str.toLetMatch exp
      args = makeVars _let.letMatchArgs
      body = makeVars _let.letMatchBody
  in Str.fromLetMatch undefined

isNonAtom :: exp.T -> Bool
isNonAtom exp = foldr (||) False ([ Str.isLetMatch exp
                                  , Str.isTuple exp
                                  , Str.isLambda exp
                                  , Str.isArrow exp
                                  , Str.isCase exp
                                  , Str.isApp exp
                                  , Str.isArgBody exp
                                  ])
checkAndTransform :: Str.Structure a => Exp.T -> a
checkAndTransform exp
 | Str.isLetMatch exp = Str.toLetMatch exp
 | Str.isTuple exp = Str.toTuple exp
 | Str.isLambda exp = Str.toLambda exp
 | Str.isArrow exp = Str.toArrow exp
 | Str.isCase exp = Str.toCase exp
 | Str.isApp exp = Str.toApp exp
 | Str.isArgBody exp = Str.toArgBody exp
