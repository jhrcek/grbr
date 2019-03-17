{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Analyse
    ( loadModuleDependencies
    ) where

import Data.Aeson
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Graph

-- input is file X obtained by running "elm-analyse --format json > X"
loadModuleDependencies :: FilePath -> IO MyGraph
loadModuleDependencies resultsFile = do
    maybeAnalysisResult <- decodeFileStrict' resultsFile
    case maybeAnalysisResult of
        Nothing -> error $ "Failed to load analysis result from file : " <> resultsFile
        Just analysisResult -> return $ toGraph analysisResult

newtype AnalysisResult = AnalysisResult
    { modules :: Modules
    } deriving (Eq, Show, Ord, Generic, FromJSON)

data Modules = Modules
    { projectModules :: [Module]
    , dependencies   :: [(Module, Module)]
    } deriving (Eq, Show, Ord, Generic, FromJSON)

newtype Module = Module [Text]
     deriving (Eq, Show, Ord, Generic, FromJSON)

moduleName :: Module -> Text
moduleName (Module xs) = Text.intercalate "." xs

toGraph :: AnalysisResult -> MyGraph
toGraph AnalysisResult{modules} =
    mkGraph nodes edges
  where
    nameToIdMap = foldr
        (\(module1, module2) map0 ->
            let map1 = insertUniqueId module1 (Map.size map0) map0
            in         insertUniqueId module2 (Map.size map1) map1
        ) Map.empty (dependencies modules)
    nodes = (\(module_, mid) -> (mid, moduleName module_)) <$> Map.toList nameToIdMap
    edges = (\(module1, module2) -> (nameToIdMap Map.! module1, nameToIdMap Map.! module2, ())) <$> dependencies modules

insertUniqueId :: Ord k => k -> v -> Map k v -> Map k v
insertUniqueId = Map.insertWith (\_newVal oldVal -> oldVal)
