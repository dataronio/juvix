module Juvix.Test.Data.Context.ShowReferences
  ( stmRecordToShowRecord,
    defToShowDef,
    ShowRecord (..),
    Definition (..),
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Lens hiding ((|>))
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Open as Open
import qualified Juvix.Context.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified ListT
import qualified StmContainers.Map as STM

type ShowSymbolMap = HashMap.T Symbol Types.SymbolInfo

-- Aren't Closed Types wonderful

-- | Definition acts like Types.Record, except that the Record type
-- with an STM map is replaced by a showable variant
data Definition term ty sumRep
  = Def (Types.Def term ty)
  | Record (ShowRecord term ty sumRep)
  | TypeDeclar
      { definitionRepr :: sumRep
      }
  | Unknown
      { definitionMTy :: Maybe ty
      }
  | Information
      { definitionInfo :: [Types.Information]
      }
  | CurrentNameSpace
  | SumCon (Types.SumT term ty)
  deriving (Show, Read, Generic, Eq)

data ShowRecord term ty sumRep = ShowRec
  { contents :: NameSpace.T (Definition term ty sumRep),
    mTy :: Maybe ty,
    openList :: [Open.TName NameSymbol.T],
    qualifiedMap :: ShowSymbolMap
  }
  deriving (Show, Read, Generic, Eq)

stmRecordToShowRecord ::
  Types.Record term ty sumRep -> IO (ShowRecord term ty sumRep)
stmRecordToShowRecord record = do
  newContents <- traverse defToShowDef (Types.recordContents record)
  newMap <- stmMapToHashMap (Types.recordQualifiedMap record)
  pure
    ShowRec
      { contents = newContents,
        mTy = Types.recordMTy record,
        openList = Types.recordOpenList record,
        qualifiedMap = newMap
      }

defToShowDef ::
  Types.Definition term ty sumRep -> IO (Definition term ty sumRep)
defToShowDef (Types.Def definition) = Def definition |> pure
defToShowDef (Types.SumCon sumcons) = SumCon sumcons |> pure
defToShowDef (Types.Unknown unknow) = Unknown unknow |> pure
defToShowDef (Types.TypeDeclar typ) = TypeDeclar typ |> pure
defToShowDef (Types.Information fo) = Information fo |> pure
defToShowDef Types.CurrentNameSpace = CurrentNameSpace |> pure
defToShowDef (Types.Record d) = stmRecordToShowRecord d >>| Record

stmMapToHashMap ::
  (Eq key, Hashable key) => STM.Map key value -> IO (HashMap.T key value)
stmMapToHashMap map =
  STM.listT map
    |> ListT.toList
    |> STM.atomically
    >>| HashMap.fromList
