-- | Shared between Types and Sum
module Juvix.Backends.LLVM.Codegen.Types.Shared
  ( SymbolTable,
    TypeTable,
    Names,
    uniqueName,
  )
where

import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST

type SymbolTable = Map.T Symbol Operand

type TypeTable = Map.T Symbol Type

-- | Mapping from Symbols to Ints that allow us to pick an unique
-- numbering to go along with a given name.
type Names = Map.T Symbol Int

-- | @uniqueName@ given a symbol and a name table, generate a new
-- unique name and give back the updated nametable.
uniqueName :: Symbol -> Names -> (Symbol, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (intern (show nm <> show ix), Map.insert nm (succ ix) ns)

instance Hashable Name
