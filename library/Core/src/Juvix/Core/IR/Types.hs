-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
  )
where

import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data T deriving (Data, Show)

Core.extendTerm "Term" [] [t|T|] $ \_ _ -> Core.defaultExtTerm

Core.extendElim "Elim" [] [t|T|] $ \_ _ -> Core.defaultExtElim

Core.extendValue "Value" [] [t|T|] $ \_ _ -> Core.defaultExtValue

Core.extendNeutral "Neutral" [] [t|T|] $ \_ _ -> Core.defaultExtNeutral

Core.extendPattern "Pattern" [] [t|T|] $ \_ _ -> Core.defaultExtPattern

usageToGlobal :: Usage.T -> Maybe Core.GlobalUsage
usageToGlobal Usage.SAny = Just Core.GSAny
usageToGlobal (Usage.SNat 0) = Just Core.GZero
usageToGlobal _ = Nothing

globalToUsage :: Core.GlobalUsage -> Usage.T
globalToUsage Core.GSAny = Usage.SAny
globalToUsage Core.GZero = Usage.SNat 0

globalName :: Core.Global extT extV primTy primVal -> NameSymbol.T
globalName (Core.GDatatype (Core.Datatype {dataName})) = dataName
globalName (Core.GDataCon (Core.DataCon {dataConName})) = dataConName
globalName (Core.GFunction (Core.Function {funName})) = funName
globalName (Core.GAbstract (Core.Abstract {absName})) = absName
