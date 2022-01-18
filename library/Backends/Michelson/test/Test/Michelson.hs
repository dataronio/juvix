{-# LANGUAGE OverloadedLists #-}

module Test.Michelson where

import Juvix.Backends.Michelson.Compilation
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.DSL.Environment as DSL
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpret
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import Juvix.Backends.Michelson.Optimisation
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Erased.Ann as J
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Type, show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage
import Morley.Michelson.Untyped as M
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String, foldr1)

--------------------------------------------------------------------------------
-- Test Abstractions
--------------------------------------------------------------------------------

runContract :: Term -> Type -> Either DSL.CompError (Contract' ExpandedOp)
runContract term _ty =
  fst (compileContract term) >>| fst

runExpr :: Term -> Either DSL.CompError EmptyInstr
runExpr term =
  fst (compileExpr term)

runContractWrap :: Term -> Type -> Either DSL.CompError (Contract' ExpandedOp)
runContractWrap term ty =
  runContract (Ann zero newTy (J.LamM [] ["gen%%%"] term)) newTy
  where
    newTy = J.Pi zero (primTy unitPair) ty

interpretExpression :: String -> AnnTerm PrimTyHR PrimValHR -> M.Value -> T.TestTree
interpretExpression label term equal =
  T.testCase
    ("[interpret] " <> label)
    (Right equal T.@=? (runExpr term >>= Interpret.dummyInterpret))

-- TODO: Switch these tests to use the interpreter (ideally through the parameterisation :) ).
shouldCompile :: String -> Term -> Type -> Text -> T.TestTree
shouldCompile label term ty contract =
  T.testCase
    ("[compile contract] " <> label)
    (Right contract T.@=? (untypedContractToSourceLine |<< runContract term ty))

shouldOptimise :: String -> Op -> Op -> T.TestTree
shouldOptimise label instr opt =
  T.testCase
    ("[optimise] " <> label)
    (opt T.@=? optimiseSingle instr)

shouldCompileExpr :: String -> Term -> T.TestTree
shouldCompileExpr label term =
  T.testCase
    ("[compile expr] " <> label)
    (isRight (fst (compileExpr term)) T.@? "failed to compile")

-- TODO replace this with semantic meaning not exact extraction meaning!
shouldCompileTo :: String -> Term -> [Op] -> T.TestTree
shouldCompileTo label term instrs =
  T.testCase
    ("[compile] " <> label)
    (DSL.ops (snd (extractTest term)) T.@?= instrs)

--------------------------------------------------------------------------------
-- Test Groups
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "Backend Michelson"
    [ constAppTest,
      pairConstantTest,
      pairNotConstantTest,
      underExactConstTest,
      underExactNonConstTest,
      -- identityFn,
      -- identityApp,
      -- identityApp2,
      --identityExpr,
      optimiseDupDrop,
      optimiseLambdaExec,
      addDoublePairTest,
      constUIntTest,
      overExactConstTest,
      overExactConstTest2,
      overExactNonConstTest,
      overExactNonConstTest2,
      identityTermTest,
      xtwiceTest1,
      xtwiceTest2,
      oddAppTest,
      ifIntTest,
      ifIntConstTest,
      nilTest,
      intListTest
    ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

constAppTest :: T.TestTree
constAppTest = interpretExpression "constApp" constApp M.ValueUnit

pairNotConstantTest :: T.TestTree
pairNotConstantTest =
  interpretExpression
    "pairNotConstant"
    pairNotConstant
    (M.ValuePair M.ValueUnit M.ValueUnit)

pairConstantTest :: T.TestTree
pairConstantTest =
  interpretExpression
    "pairConstant"
    pairConstant
    (M.ValuePair M.ValueUnit M.ValueUnit)

optimiseDupDrop :: T.TestTree
optimiseDupDrop =
  shouldOptimise
    "dup; drop"
    (Instructions.dup <> Instructions.drop)
    (M.SeqEx [])

optimiseLambdaExec :: T.TestTree
optimiseLambdaExec =
  shouldOptimise
    "lambda"
    (Instructions.lambda Untyped.unit Untyped.unit [] <> Instructions.exec)
    (M.SeqEx [])

identityExpr :: T.TestTree
identityExpr = shouldCompileExpr "identityTerm2" identityTerm2

identityExpr2 :: T.TestTree
identityExpr2 = shouldCompileExpr "identityAppExpr" identityAppExpr

identityExpr3 :: T.TestTree
identityExpr3 = shouldCompileExpr "identityAppExpr2" identityAppExpr2

identityApp2 :: T.TestTree
identityApp2 =
  shouldCompile
    "identityAppTerm2"
    identityAppTerm2
    identityType
    "parameter unit;storage unit;code { { DIG 0;CAR;DIG 0;NIL operation;PAIR } };"

unitTest :: T.TestTree
unitTest =
  shouldCompileExpr "unitExpr1" unitExpr1

identityFn :: T.TestTree
identityFn =
  shouldCompile
    "identityTerm"
    identityTerm
    identityType
    "parameter unit;storage unit;code { { DIG 0;CAR;NIL operation;PAIR } };"

identityApp :: T.TestTree
identityApp =
  shouldCompile
    "identityAppTerm"
    identityAppTerm
    identityType
    "parameter unit;storage unit;code { { DIG 0;DIG 0;CAR;NIL operation;PAIR } };"

underExactConstTest :: T.TestTree
underExactConstTest = interpretExpression "underExactConst" underExactConst M.ValueUnit

underExactNonConstTest :: T.TestTree
underExactNonConstTest = interpretExpression "underExactNonConst" underExactNonConst M.ValueUnit

addDoublePairTest :: T.TestTree
addDoublePairTest = shouldCompileTo "addDoublePairs" addDoublePairs addDoublePairsAns

constUIntTest :: T.TestTree
constUIntTest = shouldCompileTo "constUInt" constUInt constUIntAns

overExactConstTest :: T.TestTree
overExactConstTest = shouldCompileTo "overExactConst" overExactConst overExactConstAns

overExactConstTest2 :: T.TestTree
overExactConstTest2 = interpretExpression "overExactConst" overExactConst ValueUnit

overExactNonConstTest2 :: T.TestTree
overExactNonConstTest2 = interpretExpression "overExactNonConst" overExactNonConst ValueUnit

overExactNonConstTest :: T.TestTree
overExactNonConstTest = shouldCompileTo "overExactNonConst" overExactNonConst overExactNonConstAns

identityTermTest :: T.TestTree
identityTermTest = shouldCompileTo "identityTerm" identityTerm identityTermAns

xtwiceTest2 :: T.TestTree
xtwiceTest2 =
  interpretExpression "xtwice" xtwice (M.ValueInt 9)

xtwiceTest1 :: T.TestTree
xtwiceTest1 = shouldCompileTo "xtwice" xtwice xtwiceAns

oddAppTest :: T.TestTree
oddAppTest = shouldCompileTo "oddApp" oddApp oddAppAns

ifIntTest :: T.TestTree
ifIntTest = shouldCompileTo "ifInt" ifInt ifIntAns

ifIntConstTest :: T.TestTree
ifIntConstTest = shouldCompileTo "ifIntConst" ifIntConst ifIntAns

nilTest :: T.TestTree
nilTest = shouldCompileTo "nil" nil nilAns

intListTest :: T.TestTree
intListTest = shouldCompileTo "intList" intList intListAns

-- dummyTest =
--   runContract identityAppTerm2 identityType
--     >>| Interpret.dummyInterpretContract

--------------------------------------------------------------------------------
-- Terms to test against
--------------------------------------------------------------------------------

-- TODO ∷ promote to a tasty test!
extractTest :: Term -> (Either DSL.CompError M.ExpandedOp, DSL.Env)
extractTest = DSL.execMichelson . runMichelsonExpr

testRun :: (Either DSL.CompError ExpandedOp, DSL.Env)
testRun = extractTest unitExpr1

unitExpr1 :: Term
unitExpr1 = primValT one [unit] $ Constant M.ValueUnit

symbIdent :: Term
symbIdent =
  Ann one (primTy Untyped.unit) (J.AppM lamxx [unitExpr1])

lamxx :: Term
lamxx =
  Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit)) $
    J.LamM [] ["x"] lookupX

lookupX :: Term
lookupX = Ann one (primTy Untyped.unit) (J.Var "x")

constUInt :: Term
constUInt =
  Ann
    one
    ( J.Pi one (primTy Untyped.unit) $
        J.Pi
          mempty
          (primTy Untyped.int)
          $ primTy Untyped.unit
    )
    $ J.LamM [] ["x", "y"] lookupX

-- nonConstApp generates:
--   [PrimEx (PUSH @ (Type TInt :) (ValueInt 3))
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (DIG 0)
--   ,PrimEx (DIPN 1 [PrimEx DROP])]

nonConstApp :: Term
nonConstApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      constUInt
      [ push1 M.ValueUnit Untyped.unit,
        push1 (M.ValueInt 3) Untyped.int
      ]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)]
constApp :: Term
constApp =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM constUInt [unitExpr1, annIntOne 3]

pairGen :: [AnnTerm PrimTyHR PrimValHR] -> AnnTerm PrimTyHR PrimValHR
pairGen =
  Ann one (primTy (Untyped.pair unit unit))
    . J.AppM
      ( primValT one [unit, unit, unitPair] $
          Instructions.toNewPrimErr Instructions.pair
      )

pairConstant :: Term
pairConstant = pairGen [unitExpr1, unitExpr1]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PAIR : @ % %)]

pairNotConstant :: Term
pairNotConstant = pairGen [unitExpr1, push1 M.ValueUnit Untyped.unit]

-- | 'underExactGen' tests for under application of a multi argument lambda
-- then gives it the exact number of arguments
underExactGen :: Term -> Term
underExactGen x =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit) $
              primTy Untyped.unit
          )
          $ J.AppM
            ( Ann
                one
                ( J.Pi one (primTy Untyped.unit) $
                    J.Pi one (primTy Untyped.unit) $
                      primTy Untyped.unit
                )
                $ J.LamM [] ["x", "y"] lookupX
            )
            [x]
      )
      [x]

-- Generates optimal code!
underExactConst :: Term
underExactConst = underExactGen unitExpr1

-- underExactNonConst generates:

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
-- ,PrimEx (DIG 1)]

-- note the dup, this is because in the stack, we pushed it as SAny
-- if we did better constant propagation this would be free
underExactNonConst :: Term
underExactNonConst = underExactGen (push1 M.ValueUnit Untyped.unit)

-- | 'overExactGen' tests for overapplication of a multi argument lambda
-- then feeds the rest of the arguments into the inner lambda perfectly
overExactGen :: Term -> Term
overExactGen x =
  Ann
    one
    (primTy Untyped.unit)
    $ J.AppM
      ( Ann
          one
          ( J.Pi one (primTy Untyped.unit) $
              J.Pi one (primTy Untyped.unit) $
                J.Pi one (primTy Untyped.unit) $
                  primTy Untyped.unit
          )
          $ J.LamM [] ["y", "z"] $
            Ann one (J.Pi one (primTy Untyped.unit) (primTy Untyped.unit)) $
              J.LamM [] ["x"] lookupX
      )
      [x, x, x]

overExactConst :: Term
overExactConst = overExactGen unitExpr1

overExactNonConst :: Term
overExactNonConst = overExactGen (push1 M.ValueUnit Untyped.unit)

-- IdentityTerm generates

identityTerm :: Term
identityTerm =
  Ann one identityType $
    J.LamM [] ["x"] $
      Ann one (primTy (Untyped.pair opl Untyped.unit)) $
        J.AppM
          ( primValT one [opl, unit, Untyped.pair opl unit] $
              Instructions.toNewPrimErr Instructions.pair
          )
          [ primValT one [opl] $ Constant M.ValueNil,
            Ann
              one
              (primTy unit)
              $ J.AppM
                ( primValT one [unitPair, unit] $
                    Instructions.toNewPrimErr Instructions.car
                )
                [Ann one (primTy unitPair) (J.Var "x")]
          ]

intPair :: Integer -> Integer -> Term
intPair x y =
  Ann one (primFun one t) $
    J.AppM
      (primValT one t $ Instructions.toNewPrimErr Instructions.pair)
      [push1Int x, push1Int y]
  where
    t = [int, int, pairInt]

-- intPairs1 generates:
-- [PrimEx (PUSH @ (Type TInt :) (ValueInt 3))
-- ,PrimEx (PUSH @ (Type TInt :) (ValueInt 4))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PUSH @ (Type TInt :) (ValueInt 5))
-- ,PrimEx (PUSH @ (Type TInt :) (ValueInt 6))
-- ,PrimEx (PAIR : @ % %)
-- ,PrimEx (PAIR : @ % %)]

intPairs1 :: Term
intPairs1 =
  Ann (SNat 2) (primTy (Untyped.pair pairInt pairInt)) $
    J.AppM
      ( primValT one [pairInt, pairInt, Untyped.pair pairInt pairInt] $
          Instructions.toNewPrimErr Instructions.pair
      )
      [intPair 6 5, intPair 4 3]

nil :: Term
nil = primValT' one [primTyApp' List [int]] Nil

intList :: Term
intList =
  Ann one (primTyApp List [int]) $
    J.AppM
      (primValT' one t $ Instructions.toNewPrimErr Instructions.cons)
      [push1Int 3, nil]
  where
    t = [primTy' int, primTyApp' List [int], primTyApp' List [int]]

primTyApp :: RawPrimTy -> NonEmpty M.Ty -> J.Type PrimTyHR
primTyApp f xs = J.PrimTy $ primTyApp' f xs

primTyApp' :: RawPrimTy -> NonEmpty M.Ty -> PrimTyHR
primTyApp' f xs =
  App.Return
    { retTerm = Application f $ fmap PrimTy xs,
      retType = [P.STAR]
    }

-- addPairs "x"
-- [SeqEx []
-- ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
-- ,PrimEx (CAR @ %)
-- ,PrimEx (DIG 1)
-- ,PrimEx (CDR @ %)
-- ,PrimEx (ADD @)]

addPairs :: NameSymbol.T -> Term
addPairs name =
  Ann one t' $
    J.LamM [] [name] $
      Ann one (primTy int) $
        J.AppM
          ( primValT one [int, int, int] $
              Instructions.toNewPrimErr Instructions.add
          )
          [car int int xLook, cdr int int xLook]
  where
    t' = J.Pi (SNat 2) (primTy pairInt) $ primTy int
    xLook = Ann one (primTy pairInt) (J.Var name)

addDoublePairs :: Term
addDoublePairs =
  Ann one t $
    J.AppM
      ( Ann one (J.Pi (SNat 2) (primTy (Untyped.pair pairInt pairInt)) t) $
          J.LamM [] ["y"] $
            Ann one t $
              J.AppM
                ( primValT one [int, int, pairInt] $
                    Instructions.toNewPrimErr Instructions.pair
                )
                [ applyPlus (car pairInt pairInt xLook) "a",
                  applyPlus (cdr pairInt pairInt xLook) "b"
                ]
      )
      [intPairs1]
  where
    t = primTy pairInt
    xLook =
      Ann one (primTy (Untyped.pair pairInt pairInt)) (J.Var "y")
    applyPlus term name =
      Ann one (primTy int) (J.AppM (addPairs name) [term])

xtwice :: Term
xtwice =
  Ann one (primTy int) $
    J.AppM
      ( Ann
          one
          ( J.Pi mempty (primTy int) $
              J.Pi (SNat 2) (primTy int) $
                J.Pi mempty (primTy int) $
                  primTy int
          )
          $ J.LamM [] ["y", "x", "z"] $
            Ann one (primTy int) $
              J.AppM
                ( primValT one [int, int, int] $
                    Instructions.toNewPrimErr Instructions.mul
                )
                [ Ann one (primTy int) (J.Var "x"),
                  Ann one (primTy int) (J.Var "x")
                ]
      )
      [push1Int 2, pushInt (SNat 2) 3, push1Int 4]

oddApp :: Term
oddApp =
  Ann one (primTy int) $
    J.AppM
      ( Ann
          one
          ( J.Pi mempty (primTy int) $
              J.Pi (SNat 2) (primTy int) $
                J.Pi mempty (primTy int) $
                  primTy int
          )
          $ J.LamM [] ["y", "x", "z"] $
            Ann one (primTy int) $
              J.AppM
                ( Ann one (J.Pi one (primTy int) (primTy int)) $
                    J.LamM ["x"] ["a"] $
                      Ann one (primTy int) $
                        J.AppM
                          ( primValT one [int, int, int] $
                              Instructions.toNewPrimErr Instructions.mul
                          )
                          [ Ann one (primTy int) (J.Var "x"),
                            Ann one (primTy int) (J.Var "a")
                          ]
                )
                [Ann one (primTy int) (J.Var "x")]
      )
      [push1Int 2, push1Int 3, push1Int 4]

-- this should really be a pair we are sending in, but we can let it compile
-- (wrongly typed of course), by instead sending in a non constant unit
identityCall :: AnnTerm PrimTyHR PrimValHR
identityCall =
  Ann one (primTy Untyped.unit) $
    J.AppM identityTerm2 [push1Int 3]

identityTerm2 :: Term
identityTerm2 =
  Ann one identityType $
    J.LamM [] ["x"] $
      Ann one (primTy (Untyped.pair unitl Untyped.unit)) $
        J.AppM
          ( primValT one [unitl, Untyped.unit, Untyped.pair unitl Untyped.unit] $
              Instructions.toNewPrimErr Instructions.pair
          )
          -- Force the push to be a non constant. This should do nothing
          -- as it's already forced by the second
          [ push1 M.ValueNil unitl,
            car Untyped.unit Untyped.unit $
              Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit)) $
                J.Var "x"
          ]

-- [SeqEx []
--   ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
--   ,PrimEx (PUSH @ (Type (TList (Type TOperation :)) :) ValueNil)
--   ,SeqEx [PrimEx (DIG 1),PrimEx (DUP @),PrimEx (DUG 2)]
--   ,PrimEx (CAR @ %)
--   ,PrimEx (PAIR : @ % %)
--   ,PrimEx (DIPN 1 [PrimEx DROP])
--   ,PrimEx (DIPN 1 [PrimEx DROP])]

identityAppTerm :: Term
identityAppTerm =
  Ann one identityType $
    J.LamM [] ["y"] $
      Ann one (primTy (Untyped.pair opl Untyped.unit)) $
        J.AppM
          ( Ann one identityType $
              J.LamM [] ["x"] $
                Ann one (primTy (Untyped.pair opl Untyped.unit)) $
                  J.AppM
                    ( primValT one primPairTy' $
                        Instructions.toNewPrimErr Instructions.pair
                    )
                    [ primValT one [opl] $ Constant M.ValueNil,
                      car Untyped.unit Untyped.unit $
                        Ann one (primTy (Untyped.pair Untyped.unit Untyped.unit)) $
                          J.Var "x"
                    ]
          )
          [Ann one (primTy unitPair) (J.Var "y")]

identityAppExpr :: Term
identityAppExpr =
  Ann one identityType $
    J.LamM [] ["y"] $
      Ann one (primTy (Untyped.pair unitl Untyped.unit)) $
        J.AppM
          ( Ann one identityType $
              J.LamM [] ["x"] $
                Ann one (primTy (Untyped.pair unitl Untyped.unit)) $
                  J.AppM
                    ( primValT one primPairTy' $
                        Instructions.toNewPrimErr Instructions.pair
                    )
                    [ primValT one [unitl] $ Constant M.ValueNil,
                      Ann one (primTy Untyped.unit) $
                        J.AppM
                          ( primValT one [unitPair, unit] $
                              Instructions.toNewPrimErr Instructions.car
                          )
                          [Ann one (primTy unitPair) (J.Var "x")]
                    ]
          )
          [Ann one (primTy unitPair) (J.Var "y")]

identityAppTerm2 :: Term
identityAppTerm2 =
  Ann one identityType $
    J.LamM [] ["x"] $
      Ann one (primTy (Untyped.pair opl Untyped.unit)) $
        J.AppM
          ( Ann
              one
              (J.Pi one primPairTy (primTy (Untyped.pair opl Untyped.unit)))
              $ J.LamM ["x"] ["f"] $
                Ann one (primTy (Untyped.pair opl Untyped.unit)) $
                  J.AppM
                    (Ann one primPairTy (J.Var "f"))
                    [ primValT one [opl] $ Constant M.ValueNil,
                      Ann one (primTy Untyped.unit) $
                        J.AppM
                          ( primValT one [unitPair, Untyped.unit] $
                              Instructions.toNewPrimErr Instructions.car
                          )
                          [Ann one (primTy unitPair) (J.Var "x")]
                    ]
          )
          [ primValT one primPairTy' $
              Instructions.toNewPrimErr Instructions.pair
          ]

-- [SeqEx []
--   ,SeqEx [PrimEx (DIG 0),PrimEx (DUP @),PrimEx (DUG 1)]
--   ,PrimEx (CAR @ %)
--   ,PrimEx (NIL : @ (Type TOperation :))
--   ,PrimEx (DIG 1)
--   ,PrimEx (PAIR : @ % %)
--   ,PrimEx (DIPN 1 [PrimEx DROP])]

identityAppExpr2 :: Term
identityAppExpr2 =
  Ann
    one
    identityType
    $ J.LamM [] ["x"] $
      Ann one (primTy (Untyped.pair opl Untyped.unit)) $
        J.AppM
          ( Ann
              one
              (J.Pi one primPairTy (primTy (Untyped.pair unitl Untyped.unit)))
              $ J.LamM ["x"] ["f"] $
                Ann one (primTy (Untyped.pair unitl Untyped.unit)) $
                  J.AppM
                    (Ann one primPairTy (J.Var "f"))
                    [ primValT one [unitl] $ Constant M.ValueNil,
                      Ann one (primTy Untyped.unit) $
                        J.AppM
                          ( primValT one [unitPair, Untyped.unit] $
                              Instructions.toNewPrimErr Instructions.car
                          )
                          [Ann one (primTy unitPair) (J.Var "x")]
                    ]
          )
          [primValT one primPairTy' $ Instructions.toNewPrimErr Instructions.pair]

ifInt :: Term
ifInt =
  Ann
    one
    (primTy Untyped.int)
    $ J.AppM
      (if' Untyped.int)
      [true', push1Int 3, push1Int 4]

ifIntConst :: Term
ifIntConst =
  Ann
    one
    (primTy Untyped.int)
    $ J.AppM
      (if' Untyped.int)
      [true', annIntOne 3, annIntOne 4]

--------------------------------------------------------------------------------
-- Answers to Tests
--------------------------------------------------------------------------------

ifIntAns :: [Op]
ifIntAns =
  [ PrimEx (PUSH Untyped.blank (M.Ty M.TBool Untyped.blank) ValueTrue),
    PrimEx
      ( M.IF
          [PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 3) |> PrimEx]
          [PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 4) |> PrimEx]
      )
  ]

constUIntAns :: [Op]
constUIntAns =
  [ PrimEx
      ( LAMBDA
          Untyped.blank
          ( M.Ty
              ( TLambda
                  ( M.Ty
                      ( TPair
                          Untyped.blank
                          Untyped.blank
                          Untyped.blank
                          Untyped.blank
                          (M.Ty TUnit Untyped.blank)
                          ( M.Ty
                              ( TPair
                                  Untyped.blank
                                  Untyped.blank
                                  Untyped.blank
                                  Untyped.blank
                                  (M.Ty TInt Untyped.blank)
                                  (M.Ty (TPair Untyped.blank Untyped.blank Untyped.blank Untyped.blank (M.Ty TUnit Untyped.blank) (M.Ty TUnit Untyped.blank)) Untyped.blank)
                              )
                              Untyped.blank
                          )
                      )
                      Untyped.blank
                  )
                  (M.Ty TInt Untyped.blank)
              )
              Untyped.blank
          )
          (M.Ty TInt Untyped.blank)
          [ SeqEx
              [ PrimEx (DUP Untyped.blank),
                PrimEx (CAR Untyped.blank Untyped.blank),
                PrimEx (DIP [PrimEx (CDR Untyped.blank Untyped.blank)]),
                PrimEx (DIP [SeqEx []])
              ],
            PrimEx (DIG 0),
            PrimEx (DIPN 1 [PrimEx DROP])
          ]
      )
  ]

nilAns :: [Op]
nilAns = [PrimEx (NIL Untyped.blank Untyped.blank int)]

intListAns :: [Op]
intListAns =
  nilAns
    <> [ PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 3)),
         PrimEx (CONS Untyped.blank)
       ]

-- [SeqEx [PrimEx (DUP @)
--        ,PrimEx (CAR @ %),PrimEx (DIP [PrimEx (CDR @ %)])
--        ,PrimEx (DIP [SeqEx []])]
--   ,PrimEx (DIG 0)
--   ,PrimEx (DIPN 0 [PrimEx DROP])
--   ,PrimEx (DIPN 0 [PrimEx DROP])]

xtwiceAns :: [Op]
xtwiceAns =
  [ PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 4)),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 3)),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 2)),
    SeqEx
      [ PrimEx (DIG 1),
        PrimEx (DUP Untyped.blank),
        PrimEx (DUG 2)
      ],
    PrimEx (DIG 2),
    PrimEx (MUL Untyped.blank),
    PrimEx (DIPN 1 [PrimEx DROP]),
    PrimEx (DIPN 1 [PrimEx DROP])
  ]

oddAppAns :: [Op]
oddAppAns =
  [ PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 4)),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 3)),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 2)),
    SeqEx
      [ PrimEx (DIG 1),
        PrimEx (DUP Untyped.blank),
        PrimEx (DUG 2)
      ],
    PrimEx (DIG 0),
    PrimEx (DIG 2),
    PrimEx (MUL Untyped.blank),
    PrimEx (DIPN 1 [PrimEx DROP]),
    PrimEx (DIPN 1 [PrimEx DROP])
  ]

addDoublePairsAns :: [Op]
addDoublePairsAns =
  [ PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 3)),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 4)),
    PrimEx (PAIR Untyped.blank Untyped.blank Untyped.blank Untyped.blank),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 5)),
    PrimEx (PUSH Untyped.blank (M.Ty TInt Untyped.blank) (ValueInt 6)),
    PrimEx (PAIR Untyped.blank Untyped.blank Untyped.blank Untyped.blank),
    PrimEx (PAIR Untyped.blank Untyped.blank Untyped.blank Untyped.blank), -- stack: [((3,4),(5,6))]
    SeqEx
      [ PrimEx (DIG 0),
        PrimEx (DUP Untyped.blank),
        PrimEx (DUG 1) --         stack: [((3,4),(5,6)) : ((3,4),(5,6))]
      ],
    PrimEx (CDR Untyped.blank Untyped.blank), --        stack: [(3,4) : ((3,4),(5,6))]
    SeqEx
      [ PrimEx (DIG 0),
        PrimEx (DUP Untyped.blank),
        PrimEx (DUG 1) --         stack: [(3,4) : (3,4) : ((3,4),(5,6))]
      ],
    PrimEx (CDR Untyped.blank Untyped.blank), --        stack: [4 : 3 : ((3,4),(5,6))]
    PrimEx (DIG 1), --            stack: [(3,4) : 3 : ((3,4),(5,6))]
    PrimEx (CAR Untyped.blank Untyped.blank), --        stack: [3 : (3,4) : ((3,4),(5,6))]
    PrimEx (ADD Untyped.blank), --           stack: [7 : ((3,4),(5,6))]
    PrimEx (DIG 1), --            stack: [((3,4),(5,6)) : 7]
    PrimEx (CAR Untyped.blank Untyped.blank), --        stack: [(5,6) : 7]
    SeqEx
      [ PrimEx (DIG 0),
        PrimEx (DUP Untyped.blank),
        PrimEx (DUG 1) --         stack: [(5,6) : (5,6) : 7]
      ],
    PrimEx (CDR Untyped.blank Untyped.blank), --        stack: [5 : 6 : 7]
    PrimEx (DIG 1), --            stack: [(5,6) : 5 : 7]
    PrimEx (CAR Untyped.blank Untyped.blank), --        stack: [5 : (5,6) : 7]
    PrimEx (ADD Untyped.blank), --           stack: [11 : 7]
    PrimEx (PAIR Untyped.blank Untyped.blank Untyped.blank Untyped.blank) --  stack: [(11,7)]
  ]

overExactConstAns :: [Op]
overExactConstAns = [PrimEx (PUSH Untyped.blank (M.Ty TUnit Untyped.blank) ValueUnit)]

overExactNonConstAns :: [Op]
overExactNonConstAns =
  [ PrimEx (PUSH Untyped.blank (M.Ty TUnit Untyped.blank) ValueUnit),
    PrimEx (PUSH Untyped.blank (M.Ty TUnit Untyped.blank) ValueUnit),
    PrimEx (PUSH Untyped.blank (M.Ty TUnit Untyped.blank) ValueUnit),
    PrimEx (DIPN 0 [PrimEx DROP]),
    PrimEx (DIPN 0 [PrimEx DROP]),
    PrimEx (DIG 0)
  ]

-- [PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (PUSH @ (Type TUnit :) ValueUnit)
--   ,PrimEx (DIPN 1 [PrimEx DROP])
--   ,PrimEx (DIG 1)]

identityTermAns :: [Op]
identityTermAns =
  [ PrimEx
      ( LAMBDA
          Untyped.blank
          ( M.Ty
              ( TLambda
                  ( M.Ty
                      ( TPair
                          Untyped.blank
                          Untyped.blank
                          Untyped.blank
                          Untyped.blank
                          (M.Ty TUnit Untyped.blank)
                          ( M.Ty
                              ( TPair
                                  Untyped.blank
                                  Untyped.blank
                                  Untyped.blank
                                  Untyped.blank
                                  ( M.Ty
                                      ( TPair
                                          Untyped.blank
                                          Untyped.blank
                                          Untyped.blank
                                          Untyped.blank
                                          (M.Ty TUnit Untyped.blank)
                                          (M.Ty TUnit Untyped.blank)
                                      )
                                      Untyped.blank
                                  )
                                  (M.Ty TUnit Untyped.blank)
                              )
                              Untyped.blank
                          )
                      )
                      Untyped.blank
                  )
                  (M.Ty (TPair Untyped.blank Untyped.blank Untyped.blank Untyped.blank (M.Ty TUnit Untyped.blank) (M.Ty TUnit Untyped.blank)) Untyped.blank)
              )
              Untyped.blank
          )
          (M.Ty (TPair Untyped.blank Untyped.blank Untyped.blank Untyped.blank (M.Ty TUnit Untyped.blank) (M.Ty TUnit Untyped.blank)) Untyped.blank)
          [ SeqEx [],
            PrimEx (DIG 0),
            PrimEx (CAR Untyped.blank Untyped.blank),
            PrimEx (NIL Untyped.blank Untyped.blank (M.Ty TOperation Untyped.blank)),
            PrimEx (PAIR Untyped.blank Untyped.blank Untyped.blank Untyped.blank)
          ]
      )
  ]

--------------------------------------------------------------------------------
-- Type Abstractions
--------------------------------------------------------------------------------

fstTy :: Type
fstTy =
  J.Pi one (primTy unitPair) (primTy Untyped.unit)

pairTy :: Type
pairTy = primPairTy

identityType :: Type
identityType =
  J.Pi one (primTy unitPair) (primTy (Untyped.pair opl unit))

unitl :: M.Ty
unitl = Untyped.list Untyped.unit

unitPair :: M.Ty
unitPair = Untyped.pair unit unit

opl :: M.Ty
opl = Untyped.list Untyped.operation

unit :: M.Ty
unit = Untyped.unit

primFun :: Usage -> NonEmpty M.Ty -> Type
primFun π = foldr1 (J.Pi π) . fmap primTy

primPairTy' :: NonEmpty M.Ty
primPairTy' = [opl, Untyped.unit, Untyped.pair opl Untyped.unit]

primPairTy :: Type
primPairTy = primFun one primPairTy'

int :: M.Ty
int = Untyped.int

pairInt :: M.Ty
pairInt =
  Untyped.pair int int

car :: M.Ty -> M.Ty -> Term -> Term
car pairFst pairSnd pair =
  Ann
    { usage = SAny,
      type' = J.Pi one (primTy (Untyped.pair pairFst pairSnd)) (primTy pairSnd),
      term = J.AppM fun [pair]
    }
  where
    fun =
      primValT SAny [Untyped.pair pairFst pairSnd, pairSnd] $
        Instructions.toNewPrimErr Instructions.car

cdr :: M.Ty -> M.Ty -> Term -> Term
cdr pairFst pairSnd pair =
  Ann
    { usage = SAny,
      type' = J.Pi one (primTy (Untyped.pair pairFst pairSnd)) (primTy pairFst),
      term = J.AppM fun [pair]
    }
  where
    fun =
      primValT SAny [Untyped.pair pairFst pairSnd, pairFst] $
        Instructions.toNewPrimErr Instructions.cdr

--------------------------------------------------------------------------------
-- general abstractions
--------------------------------------------------------------------------------

primTy :: M.Ty -> J.Type PrimTyHR
primTy = J.PrimTy . primTy'

primTy' :: M.Ty -> PrimTyHR
primTy' ty = App.Return {retTerm = PrimTy ty, retType = [P.STAR]}

primValT' :: Usage -> NonEmpty PrimTyHR -> RawPrimVal -> Term
primValT' usage tys val =
  Ann
    { usage,
      type' = foldr1 (J.Pi usage) $ fmap J.PrimTy tys,
      term =
        J.Prim $
          App.Return
            { retType = P.PrimType $ fmap App.retTerm tys,
              retTerm = val
            }
    }

primValT :: Usage -> NonEmpty M.Ty -> RawPrimVal -> Term
primValT usage tys val =
  Ann
    { usage,
      type' = foldr1 (J.Pi usage) $ map primTy tys,
      term =
        J.Prim $
          App.Return
            { retType = P.PrimType $ fmap PrimTy tys,
              retTerm = val
            }
    }

primVal :: M.Ty -> M.Value' Op -> J.Term PrimTyHR PrimValHR
primVal ty val =
  J.Prim $ App.Return {retTerm = Constant val, retType = [PrimTy ty]}

annIntOne :: Integer -> Term
annIntOne i = primValT one [Untyped.int] $ Constant $ M.ValueInt i

pushInt :: Usage -> Integer -> AnnTerm PrimTyHR PrimValHR
pushInt usage i = pushUsage usage (M.ValueInt i) Untyped.int

push1Int :: Integer -> AnnTerm PrimTyHR PrimValHR
push1Int i = push1 (M.ValueInt i) Untyped.int

pushUsage :: Usage -> M.Value' Op -> M.Ty -> Term
pushUsage usage const ty =
  Ann
    { usage,
      type' = primTy Untyped.unit,
      term = J.AppM fun [arg]
    }
  where
    fun =
      primValT one [ty, ty] $
        Instructions.toNewPrimErr $ Instructions.push ty M.ValueNil
    arg = primValT one [ty] $ Constant const

push1 :: M.Value' Op -> M.Ty -> AnnTerm PrimTyHR PrimValHR
push1 const ty =
  Ann
    { usage = one,
      type' = primTy Untyped.unit,
      term = J.AppM fun [arg]
    }
  where
    fun =
      primValT one [ty, ty] $
        Instructions.toNewPrimErr $ Instructions.push ty M.ValueNil
    arg = primValT one [ty] $ Constant const

boolT :: M.Ty
boolT = M.Ty M.TBool Untyped.blank

true' :: AnnTerm PrimTyHR PrimValHR
true' = primValT one [boolT] $ Constant M.ValueTrue

false' :: AnnTerm PrimTyHR PrimValHR
false' = primValT one [boolT] $ Constant M.ValueFalse

if' :: M.Ty -> AnnTerm PrimTyHR PrimValHR
if' ty = primValT one [boolT, ty, ty, ty] $ Inst (M.IF [] [])
