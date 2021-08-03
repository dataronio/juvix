module Juvix.Sexp.Structure
  ( module Juvix.Sexp.Structure.Frontend
  , module Juvix.Sexp.Structure.Transition
  , module Juvix.Sexp.Structure.EffectHandlerHelpers
  , Structure
  ) where

import Juvix.Library
import qualified Juvix.Sexp.Structure.Frontend as Str
import qualified Juvix.Sexp.Structure.Transition as Str
import qualified Juvix.Sexp.Structure.EffectHandlerHelpers as Str
import qualified Juvix.Sexp as Sexp

class Structure a where
  to :: Sexp.T -> a
  from :: a -> Sexp.T

instance Structure Str.Defun where
  to = Str.toDefun
  from = Str.fromDefun

instance Structure Str.Type where
  to = Str.toType
  from = Str.fromType

instance Structure Str.PredAns where
  to = Str.toPredAns
  from = Str.fromPredAns

instance Structure Str.Cond where
  to = Str.toCond
  from = Str.fromCond

instance Structure Str.Signature where
  to = Str.toSignature
  from = Str.fromSignature

instance Structure Str.LetSignature where
  to = Str.toLetSignature
  from = Str.fromLetSignature

instance Structure Str.LetType where
  to = Str.toLetType
  from = Str.fromLetType

instance Structure Str.Let where
  to = Str.toLet
  from = Str.fromLet

instance Structure Str.Case where
  to = Str.toCase
  from = Str.fromCase

instance Structure Str.Arrow where
  to = Str.toArrow
  from = Str.fromArrow

instance Structure Str.Lambda where
  to = Str.toLambda
  from = Str.fromLambda

instance Structure Str.NameBind where
  to = Str.toNameBind
  from = Str.fromNameBind

instance Structure Str.NotPunned where
  to = Str.toNotPunned
  from = Str.fromNotPunned

instance Structure Str.Punned where
  to = Str.toPunned
  from = Str.fromPunned

instance Structure Str.Record where
  to = Str.toRecord
  from = Str.fromRecord

instance Structure Str.Infix where
  to = Str.toInfix
  from = Str.fromInfix

instance Structure Str.Open where
  to = Str.toOpen
  from = Str.fromOpen

instance Structure Str.OpenIn where
  to = Str.toOpenIn
  from = Str.fromOpenIn

instance Structure Str.Declare where
  to = Str.toDeclare
  from = Str.fromDeclare

instance Structure Str.Declaim where
  to = Str.toDeclaim
  from = Str.fromDeclaim

instance Structure Str.DefModule where
  to = Str.toDefModule
  from = Str.fromDefModule

instance Structure Str.LetModule where
  to = Str.toLetModule
  from = Str.fromLetModule

instance Structure Str.DefunMatch where
  to = Str.toDefunMatch
  from = Str.fromDefunMatch

instance Structure Str.ArgBody where
  to = Str.toArgBody
  from = Str.fromArgBody

instance Structure Str.If where
  to = Str.toIf
  from = Str.fromIf

instance Structure Str.IfNoElse where
  to = Str.toIfNoElse
  from = Str.fromIfNoElse

instance Structure Str.IfFull where
  to = Str.toIfFull
  from = Str.fromIfFull

instance Structure Str.DefunSigMatch where
  to = Str.toDefunSigMatch
  from = Str.fromDefunSigMatch

instance Structure Str.LetMatch where
  to = Str.toLetMatch
  from = Str.fromLetMatch

instance Structure Str.RecordNoPunned where
  to = Str.toRecordNoPunned
  from = Str.fromRecordNoPunned

instance Structure Str.Effect where
  to = Str.toEffect
  from = Str.fromEffect

instance Structure Str.DefHandler where
  to = Str.toDefHandler
  from = Str.fromDefHandler

instance Structure Str.LetHandler where
  to = Str.toLetHandler
  from = Str.fromLetHandler

instance Structure Str.LetOp where
  to = Str.toLetOp
  from = Str.fromLetOp

instance Structure Str.LetRet where
  to = Str.toLetRet
  from = Str.fromLetRet

instance Structure Str.Do where
  to = Str.toDo
  from = Str.fromDo

instance Structure Str.Via where
  to = Str.toVia
  from = Str.fromVia

instance Structure Str.Handler where
  to = Str.toHandler
  from = Str.fromHandler
