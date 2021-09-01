module Test.Golden where

import Control.Arrow (left)
import qualified Data.ByteString as ByteString (readFile)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Environment as Environment
import qualified Juvix.Contextify.Passes as Contextify
import qualified Juvix.Desugar.Passes as Pass
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (ModuleOpen (..), TopLevel)
import Juvix.Frontend.Types.Base (Header)
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (Parser)
import qualified Juvix.Library.Parser as J
import Juvix.Library.Test.Golden
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Translate.Pipeline.TopLevel as TopLevel
import qualified System.FilePath as FP
import Test.Tasty
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import Prelude (String, error)

juvixRootPath :: FilePath
juvixRootPath = "../../"
{-# INLINE juvixRootPath #-}

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

withJuvixStdlibPath :: FilePath -> FilePath
withJuvixStdlibPath p = juvixRootPath <> "stdlib/" <> p

withJuvixExamplesPath :: FilePath -> FilePath
withJuvixExamplesPath p = juvixRootPath <> "test/examples/" <> p
{-# INLINE withJuvixExamplesPath #-}

top :: IO TestTree
top =
  testGroup
    "golden tests"
    <$> sequence
      [parseTests, desugartests, contextTests]

--------------------------------------------------------------------------------
-- Parse Test Frame
--------------------------------------------------------------------------------

positiveTests ::
  (Applicative f) => TestName -> (FilePath -> f [TestTree]) -> f TestTree
positiveTests testName discoverFunction =
  testGroup testName
    <$> sequenceA
      [ testGroup "positive"
          <$> discoverFunction (withJuvixExamplesPath "positive")
      ]

negativeTests ::
  (Applicative f) => TestName -> (FilePath -> f [TestTree]) -> f TestTree
negativeTests testName discoverFunction =
  testGroup testName
    <$> sequenceA
      [ testGroup "negative"
          <$> discoverFunction (withJuvixExamplesPath "negative")
      ]

--------------------------------------------------------------------------------
-- Feedback
--------------------------------------------------------------------------------

type Feedback = Feedback.FeedbackT [] String IO

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

parseContract :: FilePath -> IO (Either [Char] (Header TopLevel))
parseContract file = do
  Parser.prettyParse <$> ByteString.readFile file

parseTests :: IO TestTree
parseTests =
  positiveTests "parse" (fmap (: []) . discoverGoldenTestsParseNonCompact)

desugartests :: IO TestTree
desugartests = positiveTests "desugar" discoverGoldenTestsDesugar

contextTests :: IO TestTree
contextTests = positiveTests "context" discoverGoldenTestsContext

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse,
  discoverGoldenTestsParseNonCompact ::
    -- | the directory in which to recursively look for golden tests
    FilePath ->
    IO TestTree
discoverGoldenTestsParse =
  discoverGoldenTestsCompact [".ju"] ".parsed" getGolden parseContract
discoverGoldenTestsParseNonCompact =
  discoverGoldenTests [".ju"] ".parsed" getGolden parseContract

discoverGoldenTestPasses ::
  (Eq a, Show a, Read a) => (t -> FilePath -> IO a) -> [(t, [Char])] -> FilePath -> IO [TestTree]
discoverGoldenTestPasses handleDiscoverFunction discoverPasses filePath =
  traverse callGolden discoverPasses
  where
    callGolden (passFunction, name) =
      discoverGoldenTestsCompact
        [".ju"]
        ("." <> name)
        getGolden
        (handleDiscoverFunction passFunction)
        filePath

discoverGoldenTestsDesugar ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsDesugar =
  discoverGoldenTestPasses
    (\pass -> expectSuccess . toNoQuotesCompact (handleDiscoverFunction pass))
    discoverDesugar
  where
    handleDiscoverFunction desugarPass filePath =
      desugarPass . snd <$> sexp filePath

discoverGoldenTestsContext ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsContext =
  discoverGoldenTestPasses
    (\contextPass -> expectSuccess . toNoQuotesCompact (handleDiscoverFunction contextPass))
    discoverContext
  where
    handleDiscoverFunction contextPass filePath = do
      let directory = FP.dropFileName filePath
      deps <- liftIO (fmap (directory <>) <$> findFileDependencies filePath)
      print deps
      desugaredPath <- fullyDesugarPath (filePath : (deps <> library))
      handleContextPass desugaredPath contextPass

parseOpen :: ByteString -> Either [Char] [ModuleOpen]
parseOpen =
  left P.errorBundlePretty
    . P.parse (J.eatSpaces parseDependencies) ""
    . Parser.removeComments

parseDependencies :: Parser [ModuleOpen]
parseDependencies = do
  void $ P.manyTill P.anySingle (P.lookAhead $ P.string "open")
  P.try $ P.many (J.spaceLiner Parser.moduleOpen)

fromOpen :: ModuleOpen -> FilePath
fromOpen (Open s) = List.intercalate "/" (unintern <$> NonEmpty.toList s) <> ".ju"

findFileDependencies :: FilePath -> IO [FilePath]
findFileDependencies f = do
  contract <- ByteString.readFile f
  print contract
  pure $ either (const []) (filter (\f -> not $ elem f stdlibs) . fmap fromOpen) (parseOpen contract)

stdlibs :: [FilePath]
stdlibs =
  [ "Circuit.ju",
    "Circuit/Field.ju",
    "LLVM.ju",
    "MichelsonAlias.ju",
    "Michelson.ju",
    "Prelude.ju"
  ]

library :: [FilePath]
library = withJuvixStdlibPath <$> stdlibs

----------------------------------------------------------------------
-- Pass Test lists
----------------------------------------------------------------------

discoverPrefix ::
  (Semigroup b, IsString b) => [(a, b)] -> [b] -> [(a, b)]
discoverPrefix =
  zipWith (\(function, string) prefix -> (function, prefix <> "-" <> string))

discoverDesugar :: [([Sexp.T] -> [Sexp.T], [Char])]
discoverDesugar =
  discoverPrefix
    [ (desugarModule, "desugar-module"),
      (desugarLet, "desugar-let"),
      (desugarCond, "desugar-cond"),
      (desugarIf, "desugar-if"),
      (desugarMultipleLet, "desugar-multiple-let"),
      (desugarMultipleDefun, "desugar-multiple-defun"),
      (desugarMultipleSig, "desugar-multiple-sig"),
      (desugarRemovePunnedRecords, "desugar-remove-punned-records"),
      (desugarHandlerTransform, "desugar-handler-transform")
    ]
    (fmap show ([0 ..] :: [Integer]))

discoverContext ::
  [(NonEmpty (NameSymbol.T, [Sexp.T]) -> Feedback Environment.SexpContext, [Char])]
discoverContext =
  discoverPrefix
    [ (contextifySexp, "contextify-sexp"),
      (resolveModuleContext, "resolve-module"),
      (resolveInfixContext, "resolve-infix")
    ]
    (fmap (: []) ['A' .. 'Z'])

----------------------------------------------------------------------
-- Desugar Passes
----------------------------------------------------------------------

-- here we setup the long sequence that op basically does

desugarModule,
  desugarLet,
  desugarCond,
  desugarIf,
  desugarMultipleLet,
  desugarMultipleDefun,
  desugarMultipleSig,
  desugarRemovePunnedRecords,
  desugarHandlerTransform ::
    [Sexp.T] -> [Sexp.T]
desugarModule = fmap Pass.moduleLetTransform
desugarLet = fmap Pass.moduleLetTransform . desugarModule
desugarCond = fmap Pass.condTransform . desugarLet
desugarIf = fmap Pass.ifTransform . desugarCond
desugarMultipleLet = fmap Pass.multipleTransLet . desugarIf
desugarMultipleDefun = Pass.multipleTransDefun . desugarMultipleLet
desugarMultipleSig = Pass.combineSig . desugarMultipleDefun
desugarRemovePunnedRecords = fmap Pass.removePunnedRecords . desugarMultipleSig
desugarHandlerTransform = fmap Pass.handlerTransform . desugarRemovePunnedRecords

fullDesugar :: [Sexp.T] -> [Sexp.T]
fullDesugar = desugarHandlerTransform

----------------------------------------------------------------------
-- Context Passes
----------------------------------------------------------------------

-- contextifySexp ::
--   NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext
contextifySexp ::
  (MonadIO m, MonadFail m) =>
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  m (Context.T Sexp.T Sexp.T Sexp.T)
contextifySexp names = do
  context <- liftIO (Contextify.fullyContextify names)
  case context of
    Left _err -> Feedback.fail "Not all modules included, please include more modules"
    Right ctx -> pure ctx

resolveModuleContext ::
  (MonadIO m, MonadFail m) => NonEmpty (NameSymbol.T, [Sexp.T]) -> m Environment.SexpContext
resolveModuleContext =
  runPass (contextifySexp, Contextify.resolveModule) "not valid pass"

resolveInfixContext ::
  (MonadIO m, MonadFail m) =>
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  m Environment.SexpContext
resolveInfixContext =
  "can't resolve infix symbols"
    |> runPass (resolveModuleContext, Contextify.inifixSoloPass)

runPass ::
  (MonadIO m, MonadFail m) => (t1 -> m t2, t2 -> Environment.MinimalMIO b) -> String -> t1 -> m b
runPass (previousPass, currentPass) errString names = do
  ctx <- previousPass names
  -- Only the module transform requires IO here!
  (record, _) <- liftIO $ Environment.runMIO (currentPass ctx)
  case record of
    Left _err -> Feedback.fail errString
    Right ctx -> pure ctx

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

handleContextPass ::
  (Monad m, Show ty, Show term, Show sumRep, MonadFail m) =>
  [(NonEmpty Symbol, b)] ->
  (NonEmpty (NonEmpty Symbol, b) -> m (Context.T term ty sumRep)) ->
  m (Context.Record term ty sumRep)
handleContextPass desuagredSexp contextPass =
  case desuagredSexp of
    [] ->
      Feedback.fail "error: there are no files given"
    (moduleName, moduleDefns) : xs -> do
      let nonEmptyDesugar = (moduleName, moduleDefns) NonEmpty.:| xs
      context <- contextPass nonEmptyDesugar
      getModuleName moduleName context
  where
    getModuleName name context =
      case fmap Context.extractValue $ Context.lookup name context of
        Just (Context.Record r) ->
          pure r
        maybeDef ->
          Feedback.fail ("Definition is not a Record:" <> show maybeDef)

sexp :: FilePath -> Feedback (NameSymbol.T, [Sexp.T])
sexp path = do
  fileRead <- liftIO $ Frontend.parseSingleFile path
  case fileRead of
    Right (names, top) ->
      pure (names, fmap TopLevel.transTopLevel top)
    Left _ -> Feedback.fail "failed to turn into a sexp"

fullyDesugarPath ::
  (MonadIO m, MonadFail m) => [FilePath] -> m [(NameSymbol.T, [Sexp.T])]
fullyDesugarPath paths = do
  fileRead <- liftIO $ Frontend.parseFiles paths
  case fileRead of
    Right xs ->
      pure $ fmap (\(names, top) -> (names, fullDesugar (fmap TopLevel.transTopLevel top))) xs
    Left _ -> Feedback.fail "failed to desugar"
