module Juvix.PackageManager.StdLib where

import Juvix.Library
import System.Directory
import Text.Pretty.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import GitHub (github)
import qualified GitHub

getJuvixHome :: IO FilePath
getJuvixHome = (<> "/.juvix/") <$> getHomeDirectory

remoteStrategy :: IO ()
remoteStrategy = do  
  getContents "stdlib"
  where
    createDir p = do
      d <- getJuvixHome 
      createDirectoryIfMissing True (d <> p) 

    getContents :: Text -> IO ()
    getContents path = do
      stdlibsR <- github (GitHub.OAuth "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") 
                    -- This key just has public repo read access
                    $ GitHub.contentsForR "anoma" "juvix" path Nothing

      case stdlibsR of
        Left err -> pPrint err
        Right (GitHub.ContentDirectory stdlibs) -> do
          createDir (toS path)
          traverse_ (\stdlib -> getContents (GitHub.contentPath $ GitHub.contentItemInfo stdlib)) stdlibs
        Right (GitHub.ContentFile file) -> do
          let content = GitHub.contentFileContent file
          let path = GitHub.contentPath $ GitHub.contentFileInfo file
          localJuvix <- getJuvixHome
          BS.writeFile (localJuvix <> (toS path)) (BS.decodeLenient $ encodeUtf8 content)

localHomeStrategy :: IO Bool
localHomeStrategy = do
  d <- getJuvixHome
  doesDirectoryExist d

loadStdLibs :: IO ()
loadStdLibs = do
  -- TODO: How long do we want to cache this?
  success <- localHomeStrategy
  unless success remoteStrategy

