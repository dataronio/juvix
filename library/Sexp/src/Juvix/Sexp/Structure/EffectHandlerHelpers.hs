module Juvix.Sexp.Structure.EffectHandlerHelpers where

import Juvix.Library hiding (Handler)
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Control.Lens as Lens hiding ((|>))

data Effect = Effect
  { effectName :: Sexp.T,
    effectOps :: Sexp.T
  }
  deriving (Show)

data DefHandler = DefHandler
  { defHandlerName :: Sexp.T,
    defHandlerOps :: Sexp.T
  }
  deriving (Show)

data LetHandler = LetHandler
  { letHandlerName :: Sexp.T,
    letHandlerOps :: Sexp.T,
    letHandlerRet :: Sexp.T
  }
  deriving (Show)

data LetOp = LetOp
  { letOpName :: Sexp.T,
    letOpArgs :: Sexp.T,
    letOpBody :: Sexp.T
  }
  deriving (Show)

data LetRet = LetRet
  { letRetBody :: Sexp.T
  }
  deriving (Show)

data Do = Do
  { doStatements :: Sexp.T
  }
  deriving (Show)

Lens.makeLensesWith Lens.camelCaseFields ''DefHandler
Lens.makeLensesWith Lens.camelCaseFields ''LetHandler
Lens.makeLensesWith Lens.camelCaseFields ''Effect
Lens.makeLensesWith Lens.camelCaseFields ''LetRet
Lens.makeLensesWith Lens.camelCaseFields ''LetOp
Lens.makeLensesWith Lens.camelCaseFields ''Do

----------------------------------------
-- LetHandler
----------------------------------------

nameLetHandler :: NameSymbol.T
nameLetHandler = ":let-handler"

isLetHandler :: Sexp.T -> Bool
isLetHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetHandler
isLetHandler _ = False

toLetHandler :: Sexp.T -> Maybe LetHandler
toLetHandler form
  | isLetHandler form =
    case form of
      _nameLetHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetHandler sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetHandler :: LetHandler -> Sexp.T
fromLetHandler (LetHandler sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetHandler, sexp1, sexp2, sexp3]

----------------------------------------
-- Effect
----------------------------------------

nameEffect :: NameSymbol.T
nameEffect = ":defeff"

isEffect :: Sexp.T -> Bool
isEffect (Sexp.Cons form _) = Sexp.isAtomNamed form nameEffect
isEffect _ = False

toEffect :: Sexp.T -> Maybe Effect
toEffect form
  | isEffect form =
    case form of
      _nameEffect Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Effect sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromEffect :: Effect -> Sexp.T
fromEffect (Effect sexp1 sexp2) =
  Sexp.list [Sexp.atom nameEffect, sexp1, sexp2]
----------------------------------------
-- DefHandler
----------------------------------------

nameDefHandler :: NameSymbol.T
nameDefHandler = ":defHandler"

isDefHandler :: Sexp.T -> Bool
isDefHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefHandler
isDefHandler _ = False

toDefHandler :: Sexp.T -> Maybe DefHandler
toDefHandler form
  | isDefHandler form =
    case form of
      _nameDefHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        DefHandler sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefHandler :: DefHandler -> Sexp.T
fromDefHandler (DefHandler sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDefHandler, sexp1, sexp2]

----------------------------------------
-- LetRet
----------------------------------------

nameLetRet :: NameSymbol.T
nameLetRet = ":let-return"

isLetRet :: Sexp.T -> Bool
isLetRet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetRet
isLetRet _ = False

toLetRet :: Sexp.T -> Maybe LetRet
toLetRet form
  | isLetRet form =
    case form of
      _nameLetRet Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        LetRet sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetRet :: LetRet -> Sexp.T
fromLetRet (LetRet sexp1 sexp2) =
  Sexp.list [Sexp.atom nameLetRet, sexp1, sexp2]

----------------------------------------
-- LetOp
----------------------------------------

nameLetOp :: NameSymbol.T
nameLetOp = ":let-op"

isLetOp :: Sexp.T -> Bool
isLetOp (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetOp
isLetOp _ = False

toLetOp :: Sexp.T -> Maybe LetOp
toLetOp form
  | isLetOp form =
    case form of
      _nameLetOp Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetOp sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetOp :: LetOp -> Sexp.T
fromLetOp (LetOp sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetOp, sexp1, sexp2, sexp3]

----------------------------------------
-- Do
----------------------------------------

nameDo :: NameSymbol.T
nameDo = ":do"

isDo :: Sexp.T -> Bool
isDo (Sexp.Cons form _) = Sexp.isAtomNamed form nameDo
isDo _ = False

toDo :: Sexp.T -> Maybe Do
toDo form
  | isDo form =
    case form of
      _nameDo Sexp.:> sexp1 ->
        Do sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDo :: Do -> Sexp.T
fromDo (Do sexp1) =
  Sexp.listStar [Sexp.atom nameDo, sexp1]

----------------------------------------
-- Non-generated structures
----------------------------------------

data Via = Via
  { viaHandler :: Handler,
    viaProgram :: Do
  }

data Handler = Handler
 { handlerName :: NameSymbol.T,
   handlerRet  :: LetRet,
   handlerOps  :: [LetOp]
 }

nameVia :: NameSymbol.T
nameVia = ":via"

isVia :: Sexp.T -> Bool
isVia (Sexp.Cons form _) = Sexp.isAtomNamed form nameVia
isVia _ = False

toVia :: Sexp.T -> Maybe Via
toVia form
  | isVia form =
    case form of
      _ Sexp.:> hand Sexp.:> prog Sexp.:> Sexp.Nil ->
        Via <$> toHandler hand <*> toDo prog
      _ -> Nothing
  | otherwise = Nothing

toHandler :: Sexp.T -> Maybe Handler
toHandler form =
  let hand = toLetHandler form
      ret  = letHandlerRet  <$> hand >>= toLetRet
      ops  = letHandlerOps  <$> hand >>= toLetOps
      name = letHandlerName <$> hand >>= toNameSymbol
  in Handler <$> name <*> ret <*> ops

toLetOps :: Sexp.T -> Maybe [LetOp]
toLetOps form =
  let ops = Sexp.toList form
  in undefined

toNameSymbol :: Sexp.T -> Maybe NameSymbol.T
toNameSymbol = undefined
