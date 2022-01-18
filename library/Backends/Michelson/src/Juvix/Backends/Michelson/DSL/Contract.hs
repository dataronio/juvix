module Juvix.Backends.Michelson.DSL.Contract
  ( module Juvix.Backends.Michelson.DSL.Contract,
    dummyNow,
    dummyLevel,
    dummyMaxSteps,
    dummyOrigination,
  )
where

import Juvix.Library
import qualified Morley.Michelson.Interpret as Interpret
import Morley.Michelson.Runtime.Dummy as Dummy
import qualified Morley.Michelson.TypeCheck.TypeCheck as Type
import qualified Morley.Tezos.Address as Address
import qualified Morley.Tezos.Core as Core

dummyStamp :: Core.Timestamp
dummyStamp = Core.timestampFromSeconds 1234

dummyStepsLeft :: Interpret.RemainingSteps
dummyStepsLeft = Interpret.RemainingSteps 1000

dummyMutez :: Core.Mutez
dummyMutez = Core.toMutez 123456

dummyAccounts :: Type.TcOriginatedContracts
dummyAccounts = mempty

dummyAddress :: Address.Address
dummyAddress = Address.detGenKeyAddress "dfasfooobarnhilesas"

dummySend :: Core.Mutez
dummySend = Core.toMutez 10000

dummyChainId :: Core.ChainId
dummyChainId = Core.dummyChainId

dummyGlobalCounter :: Address.GlobalCounter
dummyGlobalCounter = Address.GlobalCounter 12345678

dummyContractEnv :: Interpret.ContractEnv
dummyContractEnv =
  Dummy.dummyContractEnv
    { Interpret.ceNow = dummyStamp,
      Interpret.ceMaxSteps = dummyStepsLeft,
      Interpret.ceBalance = dummyMutez,
      Interpret.ceContracts = dummyAccounts,
      Interpret.ceSelf = dummyAddress,
      Interpret.ceSource = dummyAddress,
      Interpret.ceSender = dummyAddress,
      Interpret.ceAmount = dummySend,
      Interpret.ceChainId = dummyChainId
    }
