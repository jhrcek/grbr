{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Analyse
  ( AnalysisResult(..)
  , Modules(..)
  , Module(..)
  , getElmAnalyseReport
  , loadAnalysisResult
  , getModuleName
  ) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Prelude
import Turtle

import qualified Data.Text as Text
import qualified System.IO

loadAnalysisResult :: IO AnalysisResult
loadAnalysisResult = do
    resultsFile <- getElmAnalyseReport
    eResult <- eitherDecodeFileStrict' . Text.unpack $ format fp resultsFile
    case eResult of
        Left parseError -> die $ format ("Failed to load analysis result from file : "%fp%"\nThe error was: "%w) resultsFile parseError
        Right analysisResult -> return analysisResult

newtype AnalysisResult = AnalysisResult
    { modules :: Modules
    } deriving (Eq, Show, Ord, Generic, FromJSON)

data Modules = Modules
    { projectModules :: [Module] -- ^ modules of currently analyzed elm.json EXCLUDING dependencies
    , dependencies   :: [(Module, Module)] -- module dependencies INCLUDING modules from external deps
    } deriving (Eq, Show, Ord, Generic, FromJSON)

-- | Represents Elm module name split on '.'
newtype Module = Module [Text]
     deriving (Eq, Show, Ord, Generic, FromJSON)

getModuleName :: Module -> Text
getModuleName (Module xs) = Text.intercalate "." xs

getElmAnalyseReport :: MonadIO io => io Turtle.FilePath
getElmAnalyseReport = do
    dataDir <- getOrCreateDataDir
    let elmAnalyseReport = dataDir </> "elm-analyse.json"
    exists <- testfile elmAnalyseReport
    if exists
        then do
            generateFresh <- shouldGenerateFresh elmAnalyseReport
            when generateFresh $
                runElmAnalyse dataDir
        else runElmAnalyse dataDir
    pure elmAnalyseReport

shouldGenerateFresh :: MonadIO io => Turtle.FilePath -> io Bool
shouldGenerateFresh elmAnalyseReport = do
    modTime <- posixSecondsToUTCTime . modificationTime <$> stat elmAnalyseReport
    printf ("The output of elm-analyse is cached in "%fp%" modified at "%w%"\n\
            \I can either reuse it or generate fresh one. Should I generate fresh one? [y/N]: ")
            elmAnalyseReport modTime
    flushStdOut
    readYesOrNo False

getOrCreateDataDir :: MonadIO io => io Turtle.FilePath
getOrCreateDataDir = do
    h <- home
    let dataDir = h </> ".dep-analyzer"
    exists <- testdir dataDir
    unless exists $ mkdir dataDir
    pure dataDir

runElmAnalyse :: MonadIO io => Turtle.FilePath -> io ()
runElmAnalyse dataDir = do
    elmAnalyseExists <- isJust <$> which "elm-analyse"
    unless elmAnalyseExists $ die "elm-analyse is not present in PATH"
    {- `elm-analyse --format json` spits out entire json output as one line, which Turtle (or OS pipe) cuts off after 65536 chars
    working around that by first redirecting its stdout and stderr to temporary files and processing those instead.
     -}
    let rawOut = dataDir </> "elm-analyse.stdout"
        rawErr = dataDir </> "elm-analyse.stderr"
        outJson = dataDir </> "elm-analyse.json"
        command  = format ("elm-analyse --format json >"%fp%" 2>"%fp) rawOut rawErr
    -- ignoring exit code, because elm-analyse returns 1, whenever there are warnings
    _ <- Turtle.shell command Turtle.empty

    rawOutText <- liftIO $ readTextFile rawOut

    liftIO $ if "{\"messages\":" `Text.isInfixOf` rawOutText
        -- elm-analyse fails with non-0 exit code if it finds any code issues
        -- BUT the json output is still produced
        then writeTextFile outJson $ workAroundElmAnalyseJsonIssue rawOutText
        else printf ("Failed to retrieve json output from elm-analyse.\
               \\nstdout captured in : "%fp%
               "\nstderr captured in : "%fp%"\n") rawOut rawErr

{- When packages are not available in local elm-stuff,
the output of "elm-analyse --format json" contains extraneous lines which make the output json not valid json.
Drop everything till the beginning of the json.

Probably can be removed after this PR is merged and released
https://github.com/stil4m/elm-analyse/pull/199
-}
workAroundElmAnalyseJsonIssue :: Text -> Text
workAroundElmAnalyseJsonIssue = Text.dropWhile (/='{')

readYesOrNo :: MonadIO io => Bool -> io Bool
readYesOrNo defaultResponse = do
    mln <- readline
    case mln of
      Nothing -> pure defaultResponse
      Just ln
          | Text.null lineText -> pure defaultResponse
          | Text.isPrefixOf "y" $ Text.toLower lineText -> pure True
          | otherwise -> pure False
        where
          lineText = lineToText ln

flushStdOut :: MonadIO io => io ()
flushStdOut =  liftIO $ System.IO.hFlush System.IO.stdout
