{-# LANGUAGE ViewPatterns #-}

module Juvix.Pipeline.ToHR
  ( contextToHR,
    -- we export these functions to be able to call them stepwise from
    -- a testing place
    addSig,
    addDef,
  )
where

import qualified Data.HashMap.Strict as HM
-- import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Context as Context
import qualified Juvix.Context.Traversal as Context
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Common.Context.Traverse as Context
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import qualified Juvix.Frontend as Frontend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Pipeline.ToHR.Def as Def
import qualified Juvix.Pipeline.ToHR.Env as Env
import qualified Juvix.Pipeline.ToHR.Sig as Sig
import qualified Juvix.Pipeline.ToHR.Types as Types
import qualified Juvix.Sexp as Sexp

contextToHR ::
  ( Show primTy,
    Show primVal
  ) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  P.Parameterisation primTy primVal ->
  Either (Types.Error HR.T primTy primVal) (Core.RawGlobals HR.T primTy primVal)
contextToHR ctx param =
  Env.evalEnvEither ctx param do
    newCtx <- Context.mapSumWithName ctx attachConstructor

    let ordered = Context.recGroups newCtx

    for_ ordered \grp -> do
      traverse_ addSig grp

    for_ ordered \grp -> do
      traverse_ addDef grp
    >>| HM.mapMaybe Types.toCoreDef . Env.coreDefs
  where
    -- TODO
    -- put @"ffOrder" ordered

    -- Attaches the sum constructor with a data constructor filling
    attachConstructor s@Context.Sum {sumTDef, sumTName} dataCons c =
      case sumTDef of
        Just __ -> s
        Nothing ->
          let dataConsSexp = Sexp.atom $ NameSymbol.fromSymbol dataCons
              typeConsSexp = Sexp.atom $ NameSymbol.fromSymbol sumTName
           in s {Context.sumTDef = mkDef typeConsSexp dataConsSexp s c}
        |> Context.SumCon
        |> pure

    mkDef typeCons dataConstructor Context.Sum {sumTName} c = do
      t <- extractTypeDeclar . Context.extractValue =<< Context.lookup (NameSymbol.fromSymbol sumTName) c
      declaration <- Sexp.findKey Sexp.car dataConstructor t
      Just $
        Context.D
          { defUsage = Just Usage.SAny,
            defMTy = generateSumConsSexp typeCons declaration,
            defTerm = Sexp.list [Sexp.atom ":primitive", Sexp.atom "Builtin.Constructor"],
            defPrecedence = Context.default'
          }

    generateSumConsSexp typeCons (Sexp.cdr -> declaration) = do
      pure . sanitizeRecord $ Sexp.foldr f typeCons declaration
      where
        sanitizeRecord (x Sexp.:> fields)
          | Sexp.isAtomNamed x ":record-d" = Sexp.list $ removeFieldNames fields
        sanitizeRecord xs = xs
        removeFieldNames fields
          | Just l <- Sexp.toList (Sexp.groupBy2 fields) = Sexp.cdr <$> l
        f n acc = Sexp.list [arrow, n, acc]
        arrow = Sexp.atom "TopLevel.Prelude.->"

addSig ::
  ( Show primTy,
    Show primVal,
    HasThrow "fromFrontendError" (Types.Error HR.T primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "coreSigs" (Types.CoreSigs HR.T primTy primVal) m,
    HasState "patVars" (HM.HashMap Core.GlobalName Core.PatternVar) m
  ) =>
  Context.Entry Sexp.T Sexp.T Sexp.T ->
  m ()
addSig (Context.Entry x feDef) = do
  sigs <- Sig.transformSig x feDef
  for_ sigs $ modify @"coreSigs" . HM.insertWith Sig.mergeSigs x

addDef ::
  ( Show primTy,
    Show primVal,
    HasThrow "fromFrontendError" (Types.Error HR.T primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "coreDefs" (Types.CoreDefs HR.T primTy primVal) m,
    HasState "coreSigs" (Types.CoreSigs HR.T primTy primVal) m,
    HasState "nextPatVar" Core.PatternVar m,
    HasState "patVars" (HM.HashMap Core.GlobalName Core.PatternVar) m
  ) =>
  Context.Entry Sexp.T Sexp.T Sexp.T ->
  m ()
addDef (Context.Entry x feDef) = do
  defs <- Def.transformDef x feDef
  for_ defs \def -> do
    modify @"coreDefs" $ HM.insert (Def.defName def) def

extractTypeDeclar :: Context.Definition term ty a -> Maybe a
extractTypeDeclar (Context.TypeDeclar t) = Just t
extractTypeDeclar _ = Nothing
