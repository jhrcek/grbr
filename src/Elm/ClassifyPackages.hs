{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

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

classifyPackages :: MonadIO io => io (Map Module Package)
classifyPackages = reduce Fold.map $ do
    dir <- dirsWithElmFiles
    elmFile <- find (ends ".elm") dir
    extractPackageAndModule elmFile

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
        []   -> die $ format
            ("Failed to determine package for file:\
             \\n  file = "%fp%
             "\n  known packages = "%w) elmFile knownPackages
        more -> die $ format
            ("Multiple packages matche file:\
             \\n  file = "%fp%
             "\n  known packages = "%w%
             "\n  matched packages = "%w) elmFile knownPackages more
      where
        hits = filter (\(Package c) -> c `elem` pathPieces_) knownPackages

dirsWithElmFiles :: Shell FilePath
dirsWithElmFiles = select ["components", "client"]

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
