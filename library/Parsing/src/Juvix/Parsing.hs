module Juvix.Parsing
  ( Error (..),
    parseFiles,
    parseSingleFile,
  )
where

------------------------------------------------------------------------------

import qualified Data.ByteString as ByteString
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types as Types
import qualified System.FilePath as FilePath

------------------------------------------------------------------------------

data Error = NoHeaderErr FilePath | ParseError ParserError
  deriving (Show)

-- we abuse laziness here
-- TODO ∷ add directory option
-- this will add top level to the thing, and properly handle paths

-- | Parse multiple files into ML AST
parseFiles :: [FilePath] -> IO (Either Error [(NameSymbol.T, [Types.TopLevel])])
parseFiles =
  -- fmap gets through the IO, so that sequenceA flips the either and list
  fmap sequenceA . traverse parseSingleFile

-- | Parse single file into ML AST
parseSingleFile :: FilePath -> IO (Either Error (NameSymbol.T, [Types.TopLevel]))
parseSingleFile file = do
  read <- ByteString.readFile file
  pure $ case Parser.parse read of
    Left x ->
      Left (ParseError x)
    Right (Types.NoHeader _xs) ->
      Left (NoHeaderErr file)
    Right (Types.Header name xs) ->
      Right (name, xs)

_fileNameToModuleName :: FilePath -> NameSymbol.T
_fileNameToModuleName =
  NameSymbol.fromSymbol . intern . toUpperFirst . FilePath.takeBaseName
