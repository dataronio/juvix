module Juvix.Core.HR.Sexp where

import Control.Lens ((^.))
import qualified Juvix.Core.Base.Types.Base as Base
import qualified Juvix.Core.HR.Types as HR
import Juvix.Library hiding (from, fst, snd, to)
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure as Structure
import qualified Juvix.Sexp.Structure.CoreNamed as Named
import Juvix.Sexp.Structure.Lens

type Serialize = Structure.Structure

instance (Serialize primTy, Serialize primVal) => Structure.Structure (HR.Term primTy primVal) where
  to = deserialize
  from = serialize

instance (Serialize primTy, Serialize primVal) => Structure.Structure (HR.Elim primTy primVal) where
  to = deserializeElim
  from = serializeElim

instance Structure.Structure () where
  from () = Sexp.list []
  to (Sexp.List []) = Just ()
  to _ = Nothing

serialize ::
  (Serialize primTy, Serialize primVal) => HR.Term primTy primVal -> Sexp.T
serialize term =
  case term of
    HR.Star i ->
      case i of
        Base.U' (Base.CU nat) -> Structure.from (Named.Star (fromIntegral nat))
        -- Change the star transformer
        Base.UAny -> Sexp.list [Sexp.atom ":star", Sexp.atom ":any"]
    HR.Lam name body ->
      Structure.from (Named.Lam name (serialize body))
    HR.Let use name body rest ->
      serialize rest
        |> Named.Let (Named.Binder name (serializeUsage use) (serializeElim body))
        |> Structure.from
    HR.Pi pi name left rest ->
      serialize rest
        |> Named.Pi (Named.Binder name (serializeUsage pi) (serialize left))
        |> Structure.from
    HR.Sig pi name left rest ->
      serialize rest
        |> Named.Sigma (Named.Binder name (serializeUsage pi) (serialize left))
        |> Structure.from
    HR.PrimTy primTy ->
      Structure.from (Named.PrimTy (Structure.from primTy))
    HR.Prim prim ->
      Structure.from (Named.Prim (Structure.from prim))
    HR.Unit -> Sexp.atom ":unit"
    HR.UnitTy -> Sexp.atom ":unit-type"
    HR.Elim e -> serializeElim e
    HR.Pair p1 p2 ->
      Structure.from (Named.Pair (serialize p1) (serialize p2))
    HR.CatProduct c1 c2 ->
      Structure.from (Named.CatProduct (serialize c1) (serialize c2))
    HR.CatCoproduct c1 c2 ->
      Structure.from (Named.CatCoProduct (serialize c1) (serialize c2))
    HR.CatProductIntro c1 c2 ->
      Structure.from (Named.CatProductIntro (serialize c1) (serialize c2))
    HR.CatProductElimLeft c1 c2 ->
      Structure.from (Named.CatProductElimLeft (serialize c1) (serialize c2))
    HR.CatProductElimRight c1 c2 ->
      Structure.from (Named.CatProductElimRight (serialize c1) (serialize c2))
    HR.CatCoproductIntroLeft c1 ->
      Structure.from (Named.CatCoproductIntroLeft (serialize c1))
    HR.CatCoproductIntroRight c1 ->
      Structure.from (Named.CatCoproductIntroRight (serialize c1))
    HR.CatCoproductElim c1 c2 c3 c4 c5 ->
      serialize c5
        |> Named.CatCoproductElim (serialize c1) (serialize c2) (serialize c3) (serialize c4)
        |> Structure.from

serializeElim ::
  (Serialize primTy, Serialize primVal) => HR.Elim primTy primVal -> Sexp.T
serializeElim elim =
  case elim of
    HR.Var name -> Sexp.atom name
    HR.App fu x -> Structure.from (Named.App (serializeElim fu) (serialize x))
    HR.Ann t ty ->
      Structure.from (Named.Ann (serialize t) (serialize ty))

serializeUsage :: Usage.Usage -> Sexp.T
serializeUsage (Usage.SNat nat) = Sexp.number (fromIntegral nat)
serializeUsage Usage.SAny = Sexp.atom ":omega"

deserializeUsage :: Sexp.T -> Maybe Usage.Usage
deserializeUsage (Sexp.Atom (Sexp.N num ______)) = Just (Usage.SNat (fromIntegral num))
deserializeUsage (Sexp.Atom (Sexp.A ":omega" _)) = Just Usage.SAny
deserializeUsage _______________________________ = Nothing

deserialize ::
  (Serialize primTy, Serialize primVal) => Sexp.T -> Maybe (HR.Term primTy primVal)
deserialize expr
  | Named.isStar expr =
    -- only for Star since the number isn't accurate
    case expr of
      Sexp.List [_star, Sexp.Atom (Sexp.A ":any" _)] ->
        HR.Star Base.UAny |> Just
      Sexp.List [_star, Sexp.Atom (Sexp.N num ____)] ->
        HR.Star (Base.U' (Base.CU (fromIntegral num))) |> Just
  | Named.isLam expr = do
    lam <- Named.toLam expr
    HR.Lam (lam ^. name) <$> (deserialize (lam ^. body))
  | Named.isLet expr = do
    let' <- Named.toLet expr
    HR.Let
      <$> deserializeUsage (let' ^. binder . usage)
      <*> Just (let' ^. binder . name)
      <*> deserializeElim (let' ^. binder . term)
      <*> deserialize (let' ^. body)
  | Named.isPi expr = do
    pi <- Named.toPi expr
    HR.Pi
      <$> deserializeUsage (pi ^. binder . usage)
      <*> Just (pi ^. binder . name)
      <*> deserialize (pi ^. binder . term)
      <*> deserialize (pi ^. body)
  | Named.isSigma expr = do
    sig <- Named.toSigma expr
    HR.Sig
      <$> deserializeUsage (sig ^. binder . usage)
      <*> Just (sig ^. binder . name)
      <*> deserialize (sig ^. binder . term)
      <*> deserialize (sig ^. usage)
  | Named.isPrim expr = do
    prim' <- Named.toPrim expr
    HR.Prim <$> (Structure.to (prim' ^. prim) :: Serialize primVal => Maybe primVal)
  | Named.isPrimTy expr = do
    prim' <- Named.toPrim expr
    HR.PrimTy <$> (Structure.to (prim' ^. prim) :: Serialize primTy => Maybe primTy)
  | Sexp.atom ":unit" == expr =
    Just HR.Unit
  | Sexp.atom ":unit-type" == expr =
    Just HR.UnitTy
  | Named.isPair expr = do
    pair <- Named.toPair expr
    HR.Pair <$> deserialize (pair ^. fst) <*> deserialize (pair ^. snd)
  | Named.isCatProduct expr = do
    cat <- Named.toCatProduct expr
    HR.CatProduct <$> deserialize (cat ^. fst) <*> deserialize (cat ^. snd)
  | Named.isCatCoProduct expr = do
    (Named.CatCoProduct fst snd) <- Named.toCatCoProduct expr
    HR.CatCoproduct <$> deserialize fst <*> deserialize snd
  | Named.isCatProductIntro expr = do
    (Named.CatProductIntro fst snd) <- Named.toCatProductIntro expr
    HR.CatProductIntro <$> deserialize fst <*> deserialize snd
  | Named.isCatProductElimLeft expr = do
    (Named.CatProductElimLeft fst snd) <- Named.toCatProductElimLeft expr
    HR.CatProductElimLeft <$> deserialize fst <*> deserialize snd
  | Named.isCatProductElimRight expr = do
    (Named.CatProductElimRight fst snd) <- Named.toCatProductElimRight expr
    HR.CatProductElimRight <$> deserialize fst <*> deserialize snd
  | Named.isCatCoproductIntroLeft expr = do
    (Named.CatCoproductIntroLeft fst) <- Named.toCatCoproductIntroLeft expr
    HR.CatCoproductIntroLeft <$> deserialize fst
  | Named.isCatCoproductIntroRight expr = do
    (Named.CatCoproductIntroRight fst) <- Named.toCatCoproductIntroRight expr
    HR.CatCoproductIntroRight <$> deserialize fst
  | Named.isCatCoproductElim expr = do
    (Named.CatCoproductElim c1 c2 c3 c4 c5) <- Named.toCatCoproductElim expr
    HR.CatCoproductElim
      <$> deserialize c1
      <*> deserialize c2
      <*> deserialize c3
      <*> deserialize c4
      <*> deserialize c5
  | otherwise = HR.Elim <$> deserializeElim expr

deserializeElim ::
  (Serialize primTy, Serialize primVal) => Sexp.T -> Maybe (HR.Elim primTy primVal)
deserializeElim expr
  | Named.isAnn expr = do
    ann <- Named.toAnn expr
    HR.Ann <$> deserialize (ann ^. term) <*> deserialize (ann ^. ty)
  | otherwise = do
    case expr of
      Sexp.List _ -> do
        app <- Named.toApp expr
        HR.App <$> deserializeElim (app ^. fun) <*> deserialize (app ^. arg)
      Sexp.Atom (Sexp.A name _) ->
        Just (HR.Var name)
      _ ->
        Nothing
