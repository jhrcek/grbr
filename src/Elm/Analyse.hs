{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Analyse
    ( loadModuleDependencies
    , getNodeAndEdgeCounts
    ) where

import Data.Aeson (FromJSON, decodeFileStrict')
import Data.Graph.Inductive.Graph (mkGraph, order, size)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Graph.Types (ModuleDependencies (..), mkNodeLabel)
import System.Exit (die)

-- input is file X obtained by running "elm-analyse --format json > X"
loadModuleDependencies :: FilePath -> IO ModuleDependencies
loadModuleDependencies resultsFile = do
    maybeAnalysisResult <- decodeFileStrict' resultsFile
    case maybeAnalysisResult of
        Nothing -> die $ "Failed to load analysis result from file : " <> resultsFile <>
                        "\nIs this valid json file produced by 'elm-analyse --format json'?"
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

toGraph :: AnalysisResult -> ModuleDependencies
toGraph AnalysisResult{modules} =
    ModuleDependencies { depGraph = mkGraph nodes edges }
  where
    nameToIdMap = foldr
        (\(module1, module2) map0 ->
            let map1 = insertUniqueId module1 (Map.size map0) map0
            in         insertUniqueId module2 (Map.size map1) map1
        ) Map.empty (dependencies modules)
    appModules = IntSet.fromList $ mapMaybe (\m -> Map.lookup m nameToIdMap) (projectModules modules)
    nodes = (\(module_, mid) -> (mid, mkNodeLabel (moduleName module_) (IntSet.member mid appModules))) <$> Map.toList nameToIdMap
    edges = (\(module1, module2) -> (nameToIdMap Map.! module1, nameToIdMap Map.! module2, ())) <$> dependencies modules

insertUniqueId :: Ord k => k -> v -> Map k v -> Map k v
insertUniqueId = Map.insertWith (\_newVal oldVal -> oldVal)

getNodeAndEdgeCounts :: ModuleDependencies -> (Int, Int)
getNodeAndEdgeCounts ModuleDependencies{depGraph} =
    (order depGraph, size depGraph)
