module Test.Parameterization (top) where

import GHC.Base
import Juvix.Backends.LLVM.Compilation
import Juvix.Backends.LLVM.Parameterization
import Juvix.Backends.LLVM.Pipeline
import Juvix.Backends.LLVM.Primitive
import Juvix.Core.Parameterisation (PrimType (PrimType), hasType)
import qualified LLVM.AST.Type as LLVM
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

hasLLVMType :: RawPrimVal -> PrimType PrimTy -> Bool
hasLLVMType = hasType llvm

primTy :: LLVM.Type -> PrimType PrimTy
primTy ty = PrimType (PrimTy ty :| [])

primFunc1 :: LLVM.Type -> LLVM.Type -> PrimType PrimTy
primFunc1 ty1 ty2 = PrimType (PrimTy ty1 :| [PrimTy ty2])

primFunc2 :: LLVM.Type -> LLVM.Type -> LLVM.Type -> PrimType PrimTy
primFunc2 ty1 ty2 ty3 = PrimType (PrimTy ty1 :| [PrimTy ty2, PrimTy ty3])

binOp :: LLVM.Type -> PrimType PrimTy
binOp ty = primFunc2 ty ty ty

primInt8 :: PrimType PrimTy
primInt8 = primTy LLVM.i8

primInt16 :: PrimType PrimTy
primInt16 = primTy LLVM.i16

int8BinOp :: PrimType PrimTy
int8BinOp = binOp LLVM.i8

int16BinOp :: PrimType PrimTy
int16BinOp = binOp LLVM.i16

top :: TestTree
top = testGroup "LLVM Parameterization tests" tests

tests :: [TestTree]
tests =
  [ typecheckLLVMAddTest,
    typecheckLLVMSubTest,
    typecheckLLVMMulTest
  ]

typecheckLLVMBinOp :: TestName -> RawPrimVal -> TestTree
typecheckLLVMBinOp instrName instr =
  testCase ("Typecheck the LLVM " ++ instrName ++ " instruction") $ do
    hasLLVMType instr int8BinOp @?= True
    hasLLVMType instr int16BinOp @?= True
    hasLLVMType instr (primFunc1 LLVM.i8 LLVM.i8) @?= False
    hasLLVMType instr (primFunc1 LLVM.i16 LLVM.i16) @?= False
    hasLLVMType instr (primFunc2 LLVM.i8 LLVM.i8 LLVM.i16) @?= False
    hasLLVMType instr (primFunc2 LLVM.i8 LLVM.i16 LLVM.i8) @?= False
    hasLLVMType instr (primFunc2 LLVM.i8 LLVM.i16 LLVM.i16) @?= False
    hasLLVMType instr (primFunc2 LLVM.i16 LLVM.i8 LLVM.i8) @?= False
    hasLLVMType instr (primFunc2 LLVM.i16 LLVM.i8 LLVM.i16) @?= False
    hasLLVMType instr (primFunc2 LLVM.i16 LLVM.i16 LLVM.i8) @?= False

typecheckLLVMAddTest :: TestTree
typecheckLLVMAddTest = typecheckLLVMBinOp "add" Add

typecheckLLVMSubTest :: TestTree
typecheckLLVMSubTest = typecheckLLVMBinOp "sub" Sub

typecheckLLVMMulTest :: TestTree
typecheckLLVMMulTest = typecheckLLVMBinOp "mul" Mul
