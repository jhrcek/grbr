{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graph.Loader
    ( loadModuleGraph
    , findElmFiles
    ) where

import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Elm.Module as Elm
import qualified Graph (mkNodeLabel, moduleName)
import qualified Gwi

import Data.Graph.Inductive.Graph (LNode, labnfilter, mkGraph)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Elm.Json (loadOrDie, sourceDirectories)
import Elm.Module (defaultImports, nameToText)
import Graph (ModuleDependencies (..), NodeLabel, Package (..))
import Prelude hiding (FilePath)
import Turtle

{-| Given project directory,
 stream all elm files from the project's "source-directories"
 or die if the directory doesn't contain valid elm.json
-}
findElmFiles :: FilePath -> Shell FilePath
findElmFiles projectDir = do
    srcDirs <- sourceDirectories <$> Elm.Json.loadOrDie projectDir
    absoluteDir <- (projectDir </>) <$> select srcDirs
    ignoreTests $ find (ends ".elm") absoluteDir

ignoreTests :: Shell FilePath -> Shell FilePath
ignoreTests = mfilter (\elmFile ->
    let dirs = splitDirectories elmFile
    in  "tests/"  `notElem` dirs && "elm-stuff/" `notElem` dirs)

-- Build module dependency graph by parsing module name and imports from bunch of Elm files
foldToGraph :: FoldShell FilePath ModuleDependencies
foldToGraph =
    FoldShell processFile initState (pure . buildGraph)
  where
    initState :: FoldState
    initState = FoldState Map.empty Map.empty IMap.empty 0

    processFile :: FoldState -> FilePath -> IO FoldState
    processFile fs@FoldState{..} elmFile = do
      eitherInfo <- Elm.loadModuleInfo (encodeString elmFile)
      maybePackage <- case determinePackage elmFile of
          Left failedToDeterminePackage -> do
              printf ("[WARNING] "%s%"\n") failedToDeterminePackage
              pure Nothing
          Right pkg -> pure (Just pkg)
      case eitherInfo of
          Left _parseError -> do
              printf ("[WARNING] Failed to parse "%fp%" -> SKIPPING") elmFile
              pure fs
          Right moduleInfo -> do
              let updateFoldState = mergeModule moduleInfo maybePackage fs
              printf (d%" "%s%"\n") (processedCount + 1) (nameToText $ Elm.getName moduleInfo)
              pure updateFoldState

mergeModule :: Elm.ModuleInfo -> Maybe Package -> FoldState -> FoldState
mergeModule moduleInfo maybePackage FoldState{..} = FoldState
    { moduleToId = moduleToId_withAllTheNewStuffInserted
    , moduleToPackage = newModuleToPackage
    , moduleToImports = newModuleToImports
    , processedCount = processedCount + 1
    }
  where
    modName = Elm.getName moduleInfo
    (modId, moduleToId_withModNameInserted) = insertLookup modName moduleToId

    newModuleToImports = IMap.insert modId importedModIds moduleToImports

    imports = Elm.getImports moduleInfo
    -- TODO don't include all the modules imported by default from Core - https://elm-lang.org/0.19.0/imports#default-imports
    (importedModIds, moduleToId_withAllTheNewStuffInserted) =
        foldr (\importedModName (importedModIds0, modToId0) ->
                  let (modId1, modToId1) = insertLookup importedModName modToId0
                  in (ISet.insert modId1 importedModIds0, modToId1)
                )
              (ISet.empty, moduleToId_withModNameInserted)
              imports
    newModuleToPackage = case maybePackage of
        Nothing  -> moduleToPackage
        Just pkg -> Map.insert modId pkg moduleToPackage


type ModuleId = Int

data FoldState = FoldState
    { moduleToId      :: Map Elm.ModuleName ModuleId -- To each module we've seen (either as module name or as import) we assign unique ID
    , moduleToPackage :: Map ModuleId Package -- To modules from the currently analyzed project we attempt to assign package
    , moduleToImports :: IntMap IntSet -- Map ID of a module to set of IDs of all it's imported modules.
    , processedCount  :: Int -- count of files we processed
    }

buildGraph :: FoldState -> ModuleDependencies
buildGraph FoldState{moduleToId, moduleToPackage, moduleToImports} =
    ModuleDependencies depGraph
  where
    -- TODO look at what other ways to build the graph are in fgl - to maybe bring fold step and fold postprocessing closer
    depGraph = labnfilter isRelevantNode $ mkGraph nodes edges

    nodes = Map.foldrWithKey' (\modName modId ns ->
        let mPackage = Map.lookup modId moduleToPackage
            isAppModule = modId `Map.member` moduleToPackage
            modNameText = nameToText modName
            nodeLabel = Graph.mkNodeLabel modNameText mPackage isAppModule
        in (modId, nodeLabel) : ns
        ) [] moduleToId

    edges = IMap.foldrWithKey'
        (\modId importedModIds es ->
            [ (modId, importedModId, ()) | importedModId <- ISet.toList importedModIds ] <> es)
        [] moduleToImports

loadModuleGraph :: MonadIO io => FilePath -> io ModuleDependencies
loadModuleGraph projectDir =
    foldShell (findElmFiles projectDir) foldToGraph

insertLookup :: Ord k => k -> Map k Int -> (Int, Map k Int)
insertLookup k m = case Map.lookup k m of
    Just i -> (i, m)
    Nothing -> let i = Map.size m
               in (i, Map.insert k i m)

determinePackage :: FilePath -> Either Text Package
determinePackage elmFile = case hits of
    [c]  -> Right c
    []   -> Left $ format
        ("Failed to determine package for file "%fp%
         "\n  known packages = "%w) elmFile Gwi.knownPackages
    more -> Left $ format
        ("Multiple packages match file "%fp%
         "\n  known packages = "%w%
         "\n  matched packages = "%w) elmFile Gwi.knownPackages more
  where
    pathPieces = Text.splitOn "/" $ format fp elmFile
    hits = filter (\(Package c) -> c `elem` pathPieces) Gwi.knownPackages

-- Ignore default imports. You cannot NOT depend on them, so they clutter up the graph without adding useful info
isRelevantNode :: LNode NodeLabel -> Bool
isRelevantNode (_nodeId, label) =
    Graph.moduleName label `Set.notMember` defaultImports
