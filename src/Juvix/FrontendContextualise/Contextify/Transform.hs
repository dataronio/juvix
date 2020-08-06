{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Repr
import Juvix.Library

type Repr f =
  f (NonEmpty (Repr.FunctionLike Repr.Expression)) Repr.Signature Repr.Type

type Context =
  Repr Context.T

type Definition =
  Repr Context.Definition

contextify :: [Repr.TopLevel] -> Context
contextify = foldr updateTopLevel Context.empty

-- TODO ∷ bad hack I'll have to change
reconstructSymbol :: NonEmpty Symbol -> Symbol
reconstructSymbol =
  intern . foldr (\x acc -> unintern x <> "." <> acc) mempty

-- for now we'll drop typeclass and it's instance
-- for function top level we have to determine if it's a record
-- for ModuleOpen should we see if the definition comes after this point?!
-- if we get nothing we just stop, but I think
updateTopLevel :: Repr.TopLevel -> Context -> Context
updateTopLevel (Repr.Type t@(Repr.Typ _ name _ _)) ctx =
  Context.add name (Context.TypeDeclar t) ctx
updateTopLevel (Repr.Function (Repr.Func name f sig)) ctx =
  Context.add name (decideRecordOrDef f sig) ctx
updateTopLevel (Repr.ModuleOpen (Repr.Open mod)) ctx =
  Context.open (reconstructSymbol mod) ctx
updateTopLevel Repr.TypeClass ctx = ctx
updateTopLevel Repr.TypeClassInstance ctx = ctx

-- | decideRecordOrDef tries to figure out
-- if a given defintiion is a record or a definition
decideRecordOrDef ::
  NonEmpty (Repr.FunctionLike Repr.Expression) -> Maybe Repr.Signature -> Definition
decideRecordOrDef xs ty
  | len == 1 && emptyArgs args =
    -- For the two matched cases eventually
    -- turn these into record expressions
    case body of
      Repr.ExpRecord (Repr.ExpressionRecord i) ->
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at types
        let f (Repr.NonPunned s e) =
              Context.add
                (NonEmpty.head s)
                (decideRecordOrDef (Repr.Like [] e :| []) Nothing)
         in Context.Record (foldr f Context.empty i) ty
      Repr.Let _l ->
        def
      _ -> def
  | otherwise = def
  where
    len = length xs
    Repr.Like args body = NonEmpty.head xs
    def = Context.Def Nothing ty xs Context.default'

----------------------------------------
-- Helpers
----------------------------------------

emptyArgs :: [a] -> Bool
emptyArgs [] = True
emptyArgs (_ : _) = False