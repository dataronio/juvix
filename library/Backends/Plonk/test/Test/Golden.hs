{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Library
import Juvix.Library.BLS12381 (Fr)
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty
import Text.Pretty.Simple (pPrint)

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../../"

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

top :: IO TestTree
top =
  testGroup "Plonk golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests,
        hrTests,
        irTests,
        erasedTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "Plonk compile"
    <$> sequence
      [ compileTestsPos "test/examples/positive/circuit",
        compileTestsNeg "test/examples/negative/circuit/compile"
      ]
  where
    compileTestsPos = plonkGoldenTests ".circuit" (expectSuccess . compile)
    compileTestsNeg = plonkGoldenTests ".circuit" (expectFailure . compile)
    compile file = Plonk.compileCircuit <$> typecheck file

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "Plonk typecheck"
    <$> sequence
      [ typecheckTestsPos "test/examples/positive/circuit",
        typecheckTestsNeg "test/examples/negative/circuit/typecheck"
      ]
  where
    typecheckTestsPos = plonkGoldenTests ".typecheck" (expectSuccess . toNoQuotes typecheck)
    typecheckTestsNeg = plonkGoldenTests ".typecheck" (expectFailure . toNoQuotes typecheck)

typecheck file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parse (Plonk.BPlonk @Fr) contract
  Pipeline.typecheck @(Plonk.BPlonk Fr) context

hrTests :: IO TestTree
hrTests =
  testGroup "Plonk HR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/circuit",
        hrTestsNeg "test/examples/negative/circuit/hr"
      ]
  where
    hrTestsPos = plonkGoldenTestsNoQuotes ".hr" (expectSuccess . toNoQuotes pipelineToHR)
    hrTestsNeg = plonkGoldenTestsNoQuotes ".hr" (expectFailure . toNoQuotesEmpty pipelineToHR)

pipelineToHR file =
  do
    liftIO (readFile file)
    >>= Pipeline.toML (Plonk.BPlonk @Fr)
    >>= Pipeline.toSexp (Plonk.BPlonk @Fr)
    >>= Pipeline.toHR (Plonk.param @Fr)
    -- Reduce the Prelude related functions for readability
    >>= pure . HM.filterWithKey isNotPrelude
  where
    isNotPrelude (p NonEmpty.:| _) _ = p /= "Prelude"

pipelineToIR file = pipelineToHR file >>= Pipeline.toIR

irTests :: IO TestTree
irTests =
  testGroup "Plonk IR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/circuit",
        hrTestsNeg "test/examples/negative/circuit/ir"
      ]
  where
    hrTestsPos = plonkGoldenTestsNoQuotes ".ir" (expectSuccess . toNoQuotes pipelineToIR)
    hrTestsNeg = plonkGoldenTestsNoQuotes ".ir" (expectFailure . toNoQuotesEmpty pipelineToIR)

erasedTests :: IO TestTree
erasedTests =
  testGroup "Plonk Erased"
    <$> sequence
      [ hrTestsPos "test/examples/positive/circuit",
        hrTestsNeg "test/examples/negative/circuit/erased"
      ]
  where
    hrTestsPos = plonkGoldenTestsNoQuotes ".erased" (expectSuccess . toNoQuotes toErased)
    hrTestsNeg = plonkGoldenTestsNoQuotes ".erased" (expectFailure . toNoQuotesEmpty toErased)
    toErased file =
      do
        liftIO (readFile file)
        >>= Pipeline.toML (Plonk.BPlonk @Fr)
        >>= Pipeline.toSexp (Plonk.BPlonk @Fr)
        >>= Pipeline.toHR (Plonk.param @Fr)
        >>= Pipeline.toIR
        >>= Pipeline.toErased (Plonk.param @Fr)

    isNotPrelude (p NonEmpty.:| _) _ = p /= "Prelude"

plonkGoldenTestsNoQuotes :: [Char] -> (FilePath -> IO NoQuotes) -> FilePath -> IO TestTree
plonkGoldenTestsNoQuotes = discoverGoldenTestsNoQuotes withJuvixRootPath

plonkGoldenTests ::
  (Show a, Eq a, Read a) =>
  [Char] ->
  (FilePath -> IO a) ->
  FilePath ->
  IO TestTree
plonkGoldenTests ext f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ext getGolden f p
