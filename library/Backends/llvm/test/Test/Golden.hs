{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR.Types as HR
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty
import Prelude (String)

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../../"

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

libs :: [String]
libs = ["stdlib/Prelude.ju", "stdlib/LLVM.ju"]

top :: IO TestTree
top =
  testGroup "LLVM golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests,
        hrTests,
        irTests,
        erasedTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "LLVM compile"
    <$> sequence
      [ compileTestPos "test/examples/positive/llvm",
        compileTestNeg "test/examples/negative/llvm/compile"
      ]
  where
    compileTestPos = compileTest (expectSuccess . compile)
    compileTestNeg = compileTest (expectFailure . compile)
    compile file = LLVM.compileProgram . ErasedAnn.toRaw =<< typecheck file

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "LLVM typecheck"
    <$> sequence
      [ typecheckTestPos "test/examples/positive/llvm",
        typecheckTestNeg "test/examples/negative/llvm/typecheck"
      ]
  where
    typecheckTestPos = typecheckTest (expectSuccess . toNoQuotes typecheck)
    typecheckTestNeg = typecheckTest (expectFailure . toNoQuotesEmpty typecheck)

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.typecheck@.
typecheckTest ::
  (FilePath -> IO NoQuotes) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
typecheckTest f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".typecheck" getGolden f p

typecheck ::
  FilePath ->
  Feedback.FeedbackT [] String IO (ErasedAnn.AnnTermT LLVM.PrimTy LLVM.RawPrimVal)
typecheck file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parseWithLibs (withJuvixRootPath <$> libs) LLVM.BLLVM contract
  Pipeline.typecheck @LLVM.BLLVM context

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.llvm@.
compileTest ::
  (Show b, Eq b, Read b) =>
  (FilePath -> IO b) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
compileTest f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ".llvm" getGolden f p

hrTests :: IO TestTree
hrTests =
  testGroup "LLVM HR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/llvm",
        hrTestsNeg "test/examples/negative/llvm/hr"
      ]
  where
    hrTestsPos = llvmGoldenTestsNoQuotes ".hr" (expectSuccess . toNoQuotes pipelineToHR)
    hrTestsNeg = llvmGoldenTestsNoQuotes ".hr" (expectFailure . toNoQuotesEmpty pipelineToHR)

pipelineToHR file =
  do
    liftIO (readFile file)
    >>= Pipeline.toML' (withJuvixRootPath <$> libs) LLVM.BLLVM
    >>= Pipeline.toSexp LLVM.BLLVM
    >>= Pipeline.toHR LLVM.llvm

pipelineToIR file = pipelineToHR file >>= Pipeline.toIR

irTests :: IO TestTree
irTests =
  testGroup "LLVM IR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/llvm",
        hrTestsNeg "test/examples/negative/llvm/ir"
      ]
  where
    hrTestsPos = llvmGoldenTestsNoQuotes ".ir" (expectSuccess . toNoQuotes pipelineToIR)
    hrTestsNeg = llvmGoldenTestsNoQuotes ".ir" (expectFailure . toNoQuotesEmpty pipelineToIR)

erasedTests :: IO TestTree
erasedTests =
  testGroup "LLVM Erased"
    <$> sequence
      [ hrTestsPos "test/examples/positive/llvm",
        hrTestsNeg "test/examples/negative/llvm/erased"
      ]
  where
    hrTestsPos = llvmGoldenTestsNoQuotes ".erased" (expectSuccess . toNoQuotes toErased)
    hrTestsNeg = llvmGoldenTestsNoQuotes ".erased" (expectFailure . toNoQuotesEmpty toErased)
    toErased file =
      do
        liftIO (readFile file)
        >>= Pipeline.toML' (withJuvixRootPath <$> libs) LLVM.BLLVM
        >>= Pipeline.toSexp LLVM.BLLVM
        >>= Pipeline.toHR LLVM.llvm
        >>= Pipeline.toIR
        >>= Pipeline.toErased LLVM.llvm LLVM.Set

llvmGoldenTestsNoQuotes :: [Char] -> (FilePath -> IO NoQuotes) -> FilePath -> IO TestTree
llvmGoldenTestsNoQuotes = discoverGoldenTestsNoQuotes withJuvixRootPath

llvmGoldenTests ::
  (Show a, Eq a, Read a) =>
  [Char] ->
  (FilePath -> IO a) ->
  FilePath ->
  IO TestTree
llvmGoldenTests ext f (withJuvixRootPath -> p) = discoverGoldenTests [".ju"] ext getGolden f p
