-- |
-- This module provides a default contract environment
module Juvix.Backends.Michelson.Contract where

import qualified Morley.Michelson.Interpret as Interpt
import qualified Morley.Michelson.Runtime.Dummy as Dummy

-- TODO ∷ make a real environment
-- Though does it matter?
contract :: Interpt.ContractEnv
contract = Dummy.dummyContractEnv
