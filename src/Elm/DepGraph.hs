{-# LANGUAGE NamedFieldPuns #-}
module Elm.DepGraph
    ( loadModuleDependencies
    , getNodeAndEdgeCounts
    ) where

import Data.Graph.Inductive.Graph (mkGraph, order, size)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Elm.Analyse (AnalysisResult (..), Module, dependencies, getModuleName,
                    loadAnalysisResult, projectModules)
import Elm.ClassifyPackages
import Graph.Types (ModuleDependencies (..), mkNodeLabel)

import qualified Data.IntSet as Set
import qualified Data.Map.Strict as Map

loadModuleDependencies :: IO ModuleDependencies
loadModuleDependencies = do
    moduleToPackageMap <- Elm.ClassifyPackages.classifyPackages
    analysisResult <- Elm.Analyse.loadAnalysisResult
    pure $ toGraph moduleToPackageMap analysisResult

-- Merge info from elm-analyse (which modules are app modules + module dependencies)
-- with mapping from module to package
toGraph :: Map Module Package -> AnalysisResult -> ModuleDependencies
toGraph moduleToPackage AnalysisResult{modules} =
    ModuleDependencies { depGraph = mkGraph nodes edges }
  where
    nameToIdMap = foldr
        (\(module1, module2) map0 ->
            let map1 = insertUniqueId module1 (Map.size map0) map0
            in         insertUniqueId module2 (Map.size map1) map1
        ) Map.empty (dependencies modules)

    appModuleIds :: IntSet
    appModuleIds = Set.fromList $ mapMaybe (\module_ -> Map.lookup module_ nameToIdMap) (projectModules modules)

    nodes = (\(module_, moduleId) ->
        let mPackage = Map.lookup module_ moduleToPackage
        in
        ( moduleId
        , mkNodeLabel
            (getModuleName module_)
            mPackage
            (moduleId `Set.member` appModuleIds)
        )) <$> Map.toList nameToIdMap

    edges = (\(module1, module2) ->
        ( nameToIdMap Map.! module1
        , nameToIdMap Map.! module2
        , ()
        )) <$> dependencies modules

insertUniqueId :: Ord k => k -> v -> Map k v -> Map k v
insertUniqueId = Map.insertWith (\_newVal oldVal -> oldVal)

getNodeAndEdgeCounts :: ModuleDependencies -> (Int, Int)
getNodeAndEdgeCounts ModuleDependencies{depGraph} =
    (order depGraph, size depGraph)
