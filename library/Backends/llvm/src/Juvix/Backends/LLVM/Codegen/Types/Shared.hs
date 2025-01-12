-- | Shared between Types and Sum
module Juvix.Backends.LLVM.Codegen.Types.Shared
  ( SymbolTable,
    TypeTable,
    StringsTable,
    SumInfo (..),
    VariantToType,
    Names,
    uniqueName,
  )
where

import qualified Juvix.Backends.LLVM.Codegen.Types.CString as CString
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST

type SymbolTable = Map.T Symbol Operand

type StringsTable = Map.T CString.CString Operand

type TypeTable = Map.T Symbol Type

data SumInfo = S
  { sum' :: Symbol,
    offset :: Int,
    tagSize' :: Word32
  }
  deriving (Show, Eq)

-- | a mapping between the variant and the sum type along with
-- the tag associated with it
type VariantToType = Map.T Symbol SumInfo

-- | Mapping from Symbols to Ints that allow us to pick an unique
-- numbering to go along with a given name.
type Names = Map.T Symbol Int

-- | @uniqueName@ given a symbol and a name table, generate a new
-- unique name and give back the updated nametable.
uniqueName :: Symbol -> Names -> (Symbol, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (intern (unintern nm <> show ix), Map.insert nm (succ ix) ns)

instance Hashable Name
