module Juvix.Core.HR.Types
  ( module Juvix.Core.HR.Types,
  )
where

import Juvix.Core.Application (IsParamVar (..))
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.HR.Extend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T deriving (Data, Show)

instance IsParamVar T where
  type ParamVar T = NameSymbol.T
  freeVar _ = Just
  boundVar _ _ = Nothing

Core.extendTerm "Term" [] [t|T|] extTerm

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

pattern Sig π x s t = Sig0 π s t x

pattern Let π x l b = Let0 π l b x

{-# COMPLETE Star, PrimTy, Prim, Pi, Lam, Sig, CatProduct, CatCoproduct, CatProductIntro, CatProductElimLeft, CatProductElimRight, CatCoproductIntroLeft, CatCoproductIntroRight, CatCoproductElim, Pair, Let, UnitTy, Unit, Elim #-}

Core.extendElim "Elim" [] [t|T|] extElim

Core.extendPattern "Pattern" [] [t|T|] extPattern
