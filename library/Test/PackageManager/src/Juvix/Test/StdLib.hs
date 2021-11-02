module Juvix.Test.StdLib where

import Juvix.Library
import System.Directory
import Text.Pretty.Simple

installStdLibs :: IO ()
installStdLibs = do
    -- TODO: If found locally, do not fetch (unless flag is set to force fetch)
    getContents "stdlib"
    where
      getJuvixHome = (<> "/.juvix/") <$> getHomeDirectory
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
