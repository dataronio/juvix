module Juvix.ANF.Transform where

import Juvix.Library
import qualified Juvix.Closure as Closure
import qualified Juvix.Sexp as Exp
import qualified Juvix.Sexp.Structure as Exp
import qualified Juvix.Library.NameSymbol as NameSymbol

-- adds to context and returns name
newSymbol :: Closure.T -> Exp.T -> (NameSymbol.T, Closure.T)
newSymbol clo  = (flip Closure.insertHash) clo

makeVars = Exp.T -> Closure.T -> (Exp.T, Closure.T)
makeVars exp clo = Exp.foldPred exp isNonAtom descriminate
 where
   descriminate
     | Str.isType        exp = inToLet (transType exp clo)
     | Str.isSignature   exp = inToLet (transSig exp clo)
     | Str.isLetMatch    exp = inToLet (transLetMatch exp clo)
     | Str.isDeclare     exp = inToLet (transDeclare exp clo)
     | Str.isIf          exp = inToLet (transIf exp clo)
     | Str.isIfNoElse    exp = inToLet (transIfNoElse exp clo)
     | otherwise             = (exp, clo)
   inToLet exp clo =
     let (name, clo) = Exp.atom (newSymbol clo exp)
         args = Str.fromArgBody (Str.ArgBody exp name) -- ? does it make sense?
         body = name
     in (Str.FromLet (Str.LetMatch name args), clo)

 -- not sure how ArgBody works
transform :: Exp.T -> Closure.T -> (Exp.T, Closure.T)
transform exp clo = Exp.foldPred exp isNonAtom descriminate
 where
   descriminate
     | Str.isType        exp = transType exp clo
     | Str.isSignature   exp = transSig exp clo
     | Str.isLetMatch    exp = transLetMatch exp clo
     | Str.isIf          exp = transIf exp clo
     | Str.isIfNoElse    exp = transIfNoElse exp clo
     | otherwise             = (exp, clo)

transType exp clo =
  let _type = Str.toType exp
      (args, clo') = makeVars _type.typeArgs clo
      (body, clo'') = makeVars _type.typeBody clo'
  in (Str.fromType (Str.Type { Str.typeNameAndSig = _type.typeNameAndSig
                             , Str.typeArgs       = makeVars args
                             , Str.typeBody       = makeVars body
                             }), clo'')

transSignature exp =
  let _sig = Str.toSignature exp
      (args, clo') = makeVars _sig.signatureSig clo
  in (Str.fromSignature (Str.Signature { Str.signatureName = _sig.signatureName
                                       , Str.signatureSig  = args
                                       }), clo')

transLetMatch exp clo =
  let _let = Str.toLetMatch exp
      (args, clo') = makeVars _let.letMatchArgs clo
      (body, clo'') = makeVars _let.letMatchBody clo'
  in (Str.fromLetMatch (Str.LetMatch { Str.letMatchName = _let.letMatchName
                                     , Str.letMatchArgs = args
                                     , Str.letMatchBody = body
                                     }), clo'')
transIf exp =
  let _if = Str.toIf exp
      (pred, clo1) = makeVars _if.ifPredicate clo
      (con, clo2)  = makeVars _if.ifConclusion clo1
      (alt, clo3)  = makeVars _if.ifAlternative clo2
  in (Str.fromIf (Str.If { Str.ifPredicate   = pred
                         , Str.ifConclusion  = con
                         , Str.ifAlternative = alt
                         }), clo3)

transIfNoElse exp =
  let _if = Str.toIfNoElse exp
      (pred, clo1) = makeVars _if.iNoElsefPredicate clo
      (con, clo2)  = makeVars _if.ifNoElseConclusion clo1
  in (Str.fromIfNoElse (Str.IfNoElse { Str.ifNoElsePredicate  = pred
                                     , Str.ifNoElseConclusion = con
                                     }), clo2)

isNonAtom :: exp.T -> Bool
isNonAtom exp = foldr (||) False ([ Str.isType exp
                                  , Str.isSignature exp
                                  , Str.isLetMatch exp
                                  , Str.isIf exp
                                  , Str.isIfNoElse exp
                                  ])
