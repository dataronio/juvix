module Juvix.Sexp.Structure
  ( module Juvix.Sexp.Structure.Frontend
  , module Juvix.Sexp.Structure.Transition
  , module Juvix.Sexp.Structure.EffectHandlerHelpers
  , module Juvix.Sexp.Structure.CoreNamed
  , Structure
  , to
  , from
  ) where

import Juvix.Library hiding (Type, Handler, to, from, Meta)
import Juvix.Sexp.Structure.Frontend hiding (nameLet, isLet, fromLet, toLet, Let, letBody)
import Juvix.Sexp.Structure.Transition
import Juvix.Sexp.Structure.EffectHandlerHelpers
import Juvix.Sexp.Structure.CoreNamed hiding (Let, fromLet, toLet)
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Frontend as Frontend
import qualified Juvix.Sexp.Structure.CoreNamed as Core

class Structure a where
  to :: Sexp.T -> Maybe a
  from :: a -> Sexp.T

instance Structure Defun where
  to = toDefun
  from = fromDefun

instance Structure Type where
  to = toType
  from = fromType

instance Structure PredAns where
  to = toPredAns
  from = fromPredAns

instance Structure Cond where
  to = toCond
  from = fromCond

instance Structure Signature where
  to = toSignature
  from = fromSignature

instance Structure LetSignature where
  to = toLetSignature
  from = fromLetSignature

instance Structure LetType where
  to = toLetType
  from = fromLetType

instance Structure Frontend.Let where
  to = Frontend.toLet
  from = Frontend.fromLet

instance Structure Case where
  to = toCase
  from = fromCase

instance Structure Arrow where
  to = toArrow
  from = fromArrow

instance Structure Lambda where
  to = toLambda
  from = fromLambda

instance Structure NameBind where
  to = toNameBind
  from = fromNameBind

instance Structure NotPunned where
  to = toNotPunned
  from = fromNotPunned

instance Structure Punned where
  to = toPunned
  from = fromPunned

instance Structure Record where
  to = toRecord
  from = fromRecord

instance Structure Infix where
  to = toInfix
  from = fromInfix

instance Structure Open where
  to = toOpen
  from = fromOpen

instance Structure OpenIn where
  to = toOpenIn
  from = fromOpenIn

instance Structure Declare where
  to = toDeclare
  from = fromDeclare

instance Structure Declaim where
  to = toDeclaim
  from = fromDeclaim

instance Structure DefModule where
  to = toDefModule
  from = fromDefModule

instance Structure LetModule where
  to = toLetModule
  from = fromLetModule

instance Structure DefunMatch where
  to = toDefunMatch
  from = fromDefunMatch

instance Structure ArgBody where
  to = toArgBody
  from = fromArgBody

instance Structure If where
  to = toIf
  from = fromIf

instance Structure IfNoElse where
  to = toIfNoElse
  from = fromIfNoElse

instance Structure DefunSigMatch where
  to = toDefunSigMatch
  from = fromDefunSigMatch

instance Structure LetMatch where
  to = toLetMatch
  from = fromLetMatch

instance Structure RecordNoPunned where
  to = toRecordNoPunned
  from = fromRecordNoPunned

instance Structure Effect where
  to = toEffect
  from = fromEffect

instance Structure DefHandler where
  to = toDefHandler
  from = fromDefHandler

instance Structure LetHandler where
  to = toLetHandler
  from = fromLetHandler

instance Structure LetOp where
  to = toLetOp
  from = fromLetOp

instance Structure LetRet where
  to = toLetRet
  from = fromLetRet

instance Structure Do where
  to = toDo
  from = fromDo

instance Structure Via where
  to = toVia
  from = fromVia

instance Structure Handler where
  to = toHandler
  from = fromHandler

instance Structure Pi where
  to = toPi
  from = fromPi

instance Structure Binder where
  to = toBinder
  from = fromBinder

instance Structure Lam where
  to = toLam
  from = fromLam

instance Structure Sigma where
  to = toSigma
  from = fromSigma

instance Structure Pair where
  to = toPair
  from = fromPair

instance Structure Core.Let where
  to = Core.toLet
  from = Core.fromLet

instance Structure App where
  to = toApp
  from = fromApp

instance Structure Ann where
  to = toAnn
  from = fromAnn

instance Structure Meta where
  to = toMeta
  from = fromMeta

instance Structure RawFunClause where
  to = toRawFunClause
  from = fromRawFunClause
