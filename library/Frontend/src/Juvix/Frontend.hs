module Juvix.Frontend
  ( Error (..),
    parseFiles,
    parseSingleFile,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (toUpper)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified System.FilePath as FilePath
import Prelude (String)

data Error
  = NoHeaderErr FilePath
  | ParseError ParserError
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
  pure $
    case Parser.parse read of
      Left x ->
        Left (ParseError x)
      Right (Types.NoHeader _xs) ->
        Left (NoHeaderErr file)
      Right (Types.Header name xs) ->
        Right (name, xs)

_fileNameToModuleName :: FilePath -> NameSymbol.T
_fileNameToModuleName =
  NameSymbol.fromSymbol . intern . toUpper . FilePath.takeBaseName

toUpper :: String -> String
toUpper (x : xs) = Char.toUpper x : xs
toUpper [] = []
