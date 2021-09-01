{-# LANGUAGE DeriveAnyClass #-}

module Test.Compiler where

import Data.Field.Galois (GaloisField, PrimeField (..))
import qualified Data.Map as Map
import Juvix.Backends.Plonk (AnnTerm)
import qualified Juvix.Backends.Plonk as P
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library hiding (Type, exp)
import Juvix.Library.BLS12381 (Fr)
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Pipeline as Pipeline
import qualified Test.Example.Polynomial as Example
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: IO T.TestTree
top = do
  T.testGroup
    "Compiler tests"
    <$> sequenceA
      [ pure polynomials,
        orTests,
        andTests,
        xorTests
      ]

polynomials :: T.TestTree
polynomials = do
  T.testGroup
    "Polynomials"
    [polynomial1]

orTests :: IO T.TestTree
orTests = do
  T.testGroup "OR gate tests"
    <$> sequence [orTest [1, 1] 1, orTest [1, 0] 1, orTest [0, 1] 1, orTest [0, 0] 0]
  where
    orTest :: [Fr] -> Fr -> IO T.TestTree
    orTest = boolTest "test/Test/Example/Juvix/Or.ju"

andTests :: IO T.TestTree
andTests = do
  T.testGroup "AND gate tests"
    <$> sequence [andTest [1, 1] 1, andTest [1, 0] 0, andTest [0, 1] 0, andTest [0, 0] 0]
  where
    andTest :: [Fr] -> Fr -> IO T.TestTree
    andTest = boolTest "test/Test/Example/Juvix/And.ju"

xorTests :: IO T.TestTree
xorTests = do
  T.testGroup "XOR gate tests"
    <$> sequence [xorTest [1, 1] 0, xorTest [1, 0] 1, xorTest [0, 1] 1, xorTest [0, 0] 0]
  where
    xorTest :: [Fr] -> Fr -> IO T.TestTree
    xorTest = boolTest "test/Test/Example/Juvix/XOr.ju"

boolTest :: FilePath -> [Fr] -> Fr -> IO T.TestTree
boolTest fp inp outp = do
  Feedback.Success _ term <- Feedback.runFeedbackT $ compile fp
  pure $
    T.testCase
      (show (fromP <$> inp, fromP outp))
      (testOutput (toCircuit term) inputs outp)
  where
    inputs = mkInputs inp
    toCircuit t = P.execCircuitBuilder $ P.compileTermWithWire t

testOutput :: (GaloisField f, Bits f) => P.ArithCircuit f -> Map P.Wire f -> f -> T.Assertion
testOutput circuit inputs expectedOutput = expectedOutput T.@=? actualOutput
  where
    assignment = P.generateAssignment circuit inputs
    actualOutput = maybe (panic $ "No output found: " <> show assignment) snd (head $ Map.toList (P.assignmentOutput assignment)) -- There should be only one output

mkInputs :: [f] -> Map P.Wire f
mkInputs inputs = Map.fromList $ zipWith (\k v -> (P.InputWire k, v)) [0 ..] inputs

polynomial1 :: T.TestTree
polynomial1 = T.testCase "\\x y -> x^3 - 2x^2 + 4 = y" (testOutput Example.circuitPolynomial1 inputs output)
  where
    inputs = mkInputs [1, 3]
    output = 1 -- true!

compile :: FilePath -> Pipeline.Pipeline (AnnTerm Fr)
compile fin = do
  t <- liftIO $ readFile fin
  parsed <- Pipeline.parseWithLibs ["../../../stdlib/Prelude.ju", "../../../stdlib/Circuit.ju", "../../../stdlib/Circuit/Field.ju"] (P.BPlonk :: P.BPlonk Fr) t
  s <- Pipeline.typecheck @(P.BPlonk Fr) parsed
  pure $ ErasedAnn.toRaw s

compileOr :: Pipeline.Pipeline (AnnTerm Fr)
compileOr = compile "test/Test/Example/Juvix/Or.ju"

compileAnd :: Pipeline.Pipeline (AnnTerm Fr)
compileAnd = compile "test/Test/Example/Juvix/And.ju"

compileXor :: Pipeline.Pipeline (AnnTerm Fr)
compileXor = compile "test/TestExample/Juvix/XOr.ju"
