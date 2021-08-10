module Version
  ( branch,
    commitDate,
    hash,
    infoVersionRepo,
    progName,
    progNameVersion,
    progNameVersionTag,
    shortHash,
    version,
    versionDoc,
    versionTag,
  )
where

import Data.Char (toUpper)
import Data.String (String)
import Data.Version (Version, showVersion, versionTags)
import Development.GitRev (gitBranch, gitCommitCount, gitCommitDate, gitHash)
import Juvix.Library hiding (hash)
import Paths_juvix (version)
import System.Environment (getProgName)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x : xs) = toUpper x : xs

versionDoc :: Doc
versionDoc = text (showVersion version)

branch :: Doc
branch = $(gitBranch)

hash :: Doc
hash = $(gitHash)

commitDate :: Doc
commitDate = $(gitCommitDate)

shortHash :: Doc
shortHash = text $ take 7 $(gitHash)

versionTag :: Doc
versionTag = mconcat [versionDoc, "-", shortHash]

progName :: IO Doc
progName = text . toUpperFirst <$> getProgName

progNameVersion :: IO Doc
progNameVersion = do
  pName <- progName
  return $ mconcat [magenta pName, " version ", versionDoc]

progNameVersionTag :: IO Doc
progNameVersionTag = do
  progNameV <- progNameVersion
  return $ mconcat [white progNameV, "-", cyan shortHash]

infoVersionRepo :: IO Doc
infoVersionRepo = do
  pNameTag <- progNameVersionTag
  return $
    mconcat
      [ pNameTag,
        line,
        "Branch: ",
        green branch,
        line,
        "Commit: ",
        cyan hash,
        line,
        "Date: ",
        white commitDate,
        line
      ]