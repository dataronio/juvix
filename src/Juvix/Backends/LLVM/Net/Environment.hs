-- |
-- - Serves as the default environment for executing EAC code
module Juvix.Backends.LLVM.Net.Environment where

import qualified Juvix.Backends.LLVM.Codegen as Codegen
import qualified Juvix.Backends.LLVM.Net.EAC as EAC
import qualified Juvix.Backends.LLVM.Net.EAC.Defs as Defs
import qualified Juvix.Backends.LLVM.Net.EAC.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map

initialModule ∷
  ( Codegen.Define m,
    HasState "typTab" Codegen.TypeTable m,
    HasState "varTab" Codegen.VariantToType m
  ) ⇒
  m ()
initialModule = do
  _ ← Defs.isBothPrimary'
  _ ← Defs.findEdge'
  _ ← Defs.link'
  _ ← Defs.rewire'
  _ ← Defs.linkConnectedPort'
  _ ← EAC.fanInAux2F'
  _ ← EAC.fanInAux2A'
  _ ← EAC.fanInAux2L'
  _ ← EAC.fanInAux2E'
  Codegen.mainPort' Types.eacPointer
  Codegen.auxiliary1' Types.eacPointer
  Codegen.auxiliary2' Types.eacPointer
  Codegen.auxiliary3' Types.eacPointer
  Codegen.auxiliary4' Types.eacPointer
  -- register the hardcoded variants
  modify @"typTab" (Map.insert "numPorts" Codegen.numPorts)
  modify @"varTab"
    ( Map.insert "numPorts_small" Codegen.S
        { Codegen.sum' = "numPorts",
          Codegen.offset = 0,
          Codegen.tagSize' = 1
        }
        . Map.insert "numPorts_large" Codegen.S
          { Codegen.sum' = "numPorts",
            Codegen.offset = 1,
            Codegen.tagSize' = 1
          }
    )