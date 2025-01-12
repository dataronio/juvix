module Test.Parameterisation (top) where

import GHC.Base
import Juvix.Backends.Michelson.Compilation.Types
  ( Op,
    RawPrimTy (Application, Option, PrimTy),
    RawPrimVal (EDivI, Inst),
  )
import Juvix.Backends.Michelson.DSL.Untyped (blank)
import Juvix.Backends.Michelson.Parameterisation (michelson)
import Juvix.Core.Parameterisation (PrimType (PrimType), hasType)
import qualified Morley.Michelson.Untyped as MU
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

hasMichelsonType :: RawPrimVal -> NonEmpty RawPrimTy -> Bool
hasMichelsonType val ty = hasType michelson val (PrimType ty)

primMichelsonType :: MU.T -> MU.TypeAnn -> RawPrimTy
primMichelsonType ty ann = PrimTy (MU.Ty ty ann)

unannotatedType :: MU.T -> RawPrimTy
unannotatedType ty = primMichelsonType ty blank

unannotatedInstr :: (MU.VarAnn -> MU.InstrAbstract Op) -> RawPrimVal
unannotatedInstr instr = Inst (instr blank)

primBool :: RawPrimTy
primBool = unannotatedType MU.TBool

primNat :: RawPrimTy
primNat = unannotatedType MU.TNat

primInt :: RawPrimTy
primInt = unannotatedType MU.TInt

primTimestamp :: RawPrimTy
primTimestamp = unannotatedType MU.TTimestamp

primKeyHash :: RawPrimTy
primKeyHash = unannotatedType MU.TKeyHash

primOperation :: RawPrimTy
primOperation = unannotatedType MU.TOperation

binOp :: RawPrimTy -> NonEmpty RawPrimTy
binOp ty = ty :| [ty, ty]

natBinOp :: NonEmpty RawPrimTy
natBinOp = binOp primNat

intBinOp :: NonEmpty RawPrimTy
intBinOp = binOp primInt

lslInstr :: RawPrimVal
lslInstr = Inst (MU.LSL blank)

divInstr :: RawPrimVal
divInstr = EDivI

nowInstr :: RawPrimVal
nowInstr = unannotatedInstr MU.NOW

setDelegateInstr :: RawPrimVal
setDelegateInstr = unannotatedInstr MU.SET_DELEGATE

top :: TestTree
top = testGroup "Michelson Parameterisation tests" tests

tests :: [TestTree]
tests =
  [ typecheckLSL,
    typecheckDiv,
    typecheckNow,
    typecheckSetDelegate
  ]

typecheckLSL :: TestTree
typecheckLSL = testCase "Test type-checking of LSL instruction" $ do
  hasMichelsonType lslInstr natBinOp @?= True
  hasMichelsonType lslInstr (primNat :| [primNat, primBool]) @?= False

typecheckDiv :: TestTree
typecheckDiv = testCase "Test type-checking of DIV instruction" $ do
  hasMichelsonType divInstr natBinOp @?= True
  hasMichelsonType divInstr intBinOp @?= True
  hasMichelsonType divInstr (primNat :| [primInt, primInt]) @?= False

-- This would currently fail because typechecking of DIV is too liberal
-- (https://github.com/heliaxdev/juvix/issues/819).
-- hasMichelsonType divInstr (primString :| [primString, primMutez]) @?= False

typecheckNow :: TestTree
typecheckNow = testCase "Test type-checking of NOW instruction" $ do
  hasMichelsonType nowInstr (primTimestamp :| []) @?= True
  hasMichelsonType nowInstr (primNat :| []) @?= False

typecheckSetDelegate :: TestTree
typecheckSetDelegate = testCase "Test type-checking of SET_DELEGATE instruction" $ do
  hasMichelsonType setDelegateInstr (Application Option (primKeyHash :| []) :| [primOperation]) @?= True
  hasMichelsonType setDelegateInstr (primKeyHash :| [primOperation]) @?= False
  hasMichelsonType setDelegateInstr (primTimestamp :| []) @?= False
