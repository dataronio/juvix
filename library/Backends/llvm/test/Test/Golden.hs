{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../../"

libs :: [[Char]]
libs = ["stdlib/Prelude.ju", "stdlib/LLVM.ju"]

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

top :: IO TestTree
top =
  testGroup "LLVM golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "LLVM compile"
    <$> sequence
      [ compileTestPos "test/examples/positive/llvm",
        compileTestNeg "test/examples/negative/llvm"
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
        typecheckTestNeg "test/examples/negative/llvm"
      ]
  where
    typecheckTestPos = typecheckTest (expectSuccess . toNoQuotes typecheck)
    typecheckTestNeg = typecheckTest (expectFailure . toNoQuotes typecheck)

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
  Feedback.FeedbackT
    []
    [Char]
    IO
    (ErasedAnn.AnnTermT LLVM.PrimTy LLVM.RawPrimVal)
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
