{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Elm.ClassifyPackages
    ( Package(..)
    , classifyPackages
    , knownPackages
    ) where

import Data.Aeson (ToJSON)
import Data.Char (isUpper)
import Data.Coerce (coerce)
import Prelude hiding (FilePath)
import Turtle
import Data.Map.Strict (Map)
import Elm.Analyse (Module(..), getModuleName)
import qualified Control.Foldl as Fold
import qualified Data.Text as Text
import Elm.Json (sourceDirectories, ElmJson, Error(..))
import qualified Elm.Json

classifyPackages :: MonadIO io => io (Map Module Package)
classifyPackages = reduce Fold.map $ do
    dirs <- liftIO $ sourceDirectories <$> loadOrDie
    dir <- select dirs
    elmFile <- find (ends ".elm") dir
    extractPackageAndModule elmFile

loadOrDie :: IO ElmJson
loadOrDie =
    Elm.Json.load "elm.json" >>= \case
        Left (ElmJsonDoesNotExist _) -> die "I didn't find elm.json in the current directory"
        Left (ElmJsonDecodeError decodeError) -> die $ "Failed to decode elm.json: " <> Text.pack decodeError
        Right xs -> pure xs

-- | Represents elm package name
newtype Package = Package Text
    deriving (Eq, Ord, Show, ToJSON) via Text

{- > extractPackaeAndModule "client/crosstab-builder/QueryBuilder/Label.elm"
   (ElmModule "QueryBuilder.Label", Package "crosstab-builder")
-}
extractPackageAndModule :: FilePath -> Shell (Module, Package)
extractPackageAndModule elmFile = do
    elmModule <- toModule pathPieces
    elmPackage <- toPackage pathPieces
    return (elmModule, elmPackage)
  where
    pathPieces = Text.splitOn "/" . format fp $ dropExtension elmFile

    toModule :: [Text] -> Shell Module
    toModule pathPieces_ = do
        let moduleFromFileName = Module $ dropWhile (not . isUpper . Text.head) pathPieces_
        maybeFirstLine <- input elmFile & reduce Fold.head
        moduleFromFile <- case maybeFirstLine of
            Nothing -> die $ format ("Empty file: "%fp) elmFile
            Just firstLine -> case Text.words $ lineToText firstLine of
                ("module":moduleName:_) -> return $ Module $ Text.splitOn "." moduleName
                ("port":"module":moduleName:_) -> return $ Module $ Text.splitOn "." moduleName
                _ -> die $ format ("Failed to determine module name in "%fp) elmFile
        when (moduleFromFileName /= moduleFromFile) $
            printf ("WARNING: File name and module name don't match for "%fp%
                    "\n    module from file name = "%s%
                    "\n    module from file      = "%s%"\n") elmFile
                    (getModuleName moduleFromFileName)
                    (getModuleName moduleFromFile)
        pure moduleFromFile

    toPackage :: [Text] -> Shell Package
    toPackage pathPieces_ = case hits of
        [c]  -> return c
        []   -> do
            printf ("Failed to determine package for file:\
                   \\n  file = "%fp%
                   "\n  known packages = "%w) elmFile knownPackages
            pure $ Package "<unknown>"
             
        more -> die $ format
            ("Multiple packages matche file:\
             \\n  file = "%fp%
             "\n  known packages = "%w%
             "\n  matched packages = "%w) elmFile knownPackages more
      where
        hits = filter (\(Package c) -> c `elem` pathPieces_) knownPackages

knownPackages :: [Package]
knownPackages = coerce
    [ "_factories" :: Text
    , "_share"
    , "app-monolithic"
    , "audience-builder"
    , "chart-builder"
    , "components"
    , "dashboards"
    , "fullscreen-search"
    , "gwiq"
    , "products"
    , "crosstab-builder"
    , "settings"
    , "tv-elm"
    , "tv-study"
    ]
