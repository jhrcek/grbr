{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Json
    ( ElmJson
    , Error
    , loadEither
    , loadOrDie
    , sourceDirectories
    ) where

import qualified Data.NonEmptyList as NonEmpty
import qualified Data.Text as Text
import qualified Elm.Outline as Outline
import qualified Reporting.Exit as Exit

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Elm.Outline (Outline)
import Prelude hiding (FilePath)
import Turtle (FilePath, decodeString, die, encodeString, format, fp, testfile,
               (</>))

newtype ElmJson = ElmJson
    { _elm_json_source_directories :: [FilePath]
    }

sourceDirectories :: ElmJson -> [FilePath]
sourceDirectories = _elm_json_source_directories

data Error
    = ElmJsonDoesNotExist FilePath
    | ElmJsonDecodeError String

loadOrDie :: MonadIO io => FilePath -> io ElmJson
loadOrDie projectDir =
    loadEither projectDir >>= dieOnFailure

dieOnFailure :: MonadIO io => Either Error ElmJson -> io ElmJson
dieOnFailure = \case
    Left (ElmJsonDoesNotExist dir) -> die $ "I didn't find elm.json in the current directory: " <> format fp dir
    Left (ElmJsonDecodeError decodeError) -> die $ "Decoding elm.json failed: " <> Text.pack decodeError
    Right xs -> pure xs

loadEither :: MonadIO io => FilePath -> io (Either Error ElmJson)
loadEither projectDir = liftIO $ do
    exists <- testfile $ projectDir </> "elm.json"
    if exists
        then loadExisting projectDir
        else pure . Left $ ElmJsonDoesNotExist projectDir

loadExisting :: FilePath -> IO (Either Error ElmJson)
loadExisting elmJson =
    bimap extractError extractSourceDirs
    <$> Outline.read (encodeString elmJson)

extractError :: Exit.Outline -> Error
extractError _ = --TODO add more details
    ElmJsonDecodeError "It is not valid elm.json"

extractSourceDirs :: Outline -> ElmJson
extractSourceDirs outline = ElmJson $ case outline of
    Outline.App appOutline -> fmap convertSrcDir . NonEmpty.toList $ Outline._app_source_dirs appOutline
    Outline.Pkg _ -> ["src"]

convertSrcDir :: Outline.SrcDir -> FilePath
convertSrcDir = \case
    Outline.AbsoluteSrcDir dir -> decodeString dir
    Outline.RelativeSrcDir dir -> decodeString dir
