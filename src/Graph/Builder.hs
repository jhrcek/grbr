{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph.Builder
    ( GeneratorParams (..)
    , generateGraphFile
    , getNeighborhood
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Graph.Inductive.Basic as G
import Data.Graph.Inductive.Graph (Node, gelem, neighbors, subgraph)
import qualified Data.Graph.Inductive.Graph as G
import Data.GraphViz (GraphvizParams, NodeCluster (C, N), clusterBy, clusterID,
                      defaultParams, fmtCluster, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Attributes (fillColor, filled, shape, style)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (..))
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir, URL),
                                          Label (StrLabel), RankDir (FromLeft),
                                          Shape (Box3D, BoxShape, Ellipse))
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised (GlobalAttributes (GraphAttrs),
                                        GraphID (Str))
import Data.Maybe (isJust)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (fromStrict, pack)
import Graph.Types (ClusterLabel, DepGraph, EdgeLabel, ModuleDependencies (..),
                    NodeLabel, isAppModule, moduleName, packageName)

generateGraphFile :: MonadIO io => GeneratorParams -> ModuleDependencies -> io FilePath
generateGraphFile genParams ModuleDependencies{depGraph} =
     liftIO $ runGraphviz dotGraph Svg "graph.svg"
   where
     gvParams = graphVizParams genParams
     dotGraph = applyTred $ graphToDot gvParams $ restrictToUndesirableShareDeps depGraph
     applyTred = if enableTransitiveReduction genParams then transitiveReduction else id

{- TODO remove - investigating all the dependencies of the form
 [module from _share] -> [module NOT from _share]
-}
restrictToUndesirableShareDeps :: DepGraph -> DepGraph
restrictToUndesirableShareDeps = filterNodes . filterEdges
  where
    filterNodes g = G.nfilter (\n -> G.deg g n > 0) g

    filterEdges g = G.efilter (\(fromNode, toNode, ()) ->
        let Just fromLab = G.lab g fromNode
            Just toLab = G.lab g toNode
        in    packageName fromLab == Just "_share"
           && packageName toLab /= Just "_share"
           && isJust (packageName toLab)) g

getNeighborhood :: Node -> ModuleDependencies -> Maybe ModuleDependencies
getNeighborhood nodeId md@ModuleDependencies{depGraph}
    | nodeId `gelem` depGraph = Just $ md { depGraph = subgraph (nodeId : neighbors depGraph nodeId) depGraph }
    | otherwise = Nothing

type GvParams = GraphvizParams Node NodeLabel EdgeLabel ClusterLabel NodeLabel

graphVizParams :: GeneratorParams -> GvParams
graphVizParams GeneratorParams{centralNode, clusteringEnabled} = defaultParams
    { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
    , fmtNode = \(nodeId, nodeLabel) ->
        [ URL $ "/node/" <> pack (show nodeId)
        , toLabelAttr clusteringEnabled nodeLabel
        , toColorAttr nodeLabel
        , toShapeAttr centralNode nodeId nodeLabel
        , style filled
        ]
    , clusterBy = if clusteringEnabled then clusterByPackage else clusterBy defaultParams
    , clusterID = if clusteringEnabled then Str .fromStrict else clusterID defaultParams
    , fmtCluster = if clusteringEnabled then \pkgName -> [ GraphAttrs [ Label (StrLabel (fromStrict pkgName))]] else fmtCluster defaultParams
    }

clusterByPackage :: (Node, NodeLabel)  -> NodeCluster ClusterLabel (Node, NodeLabel)
clusterByPackage pair@(_, nodeLabel) = case packageName nodeLabel of
    Nothing      -> N pair
    Just pkgName -> C pkgName $ N pair

toShapeAttr :: Maybe Node -> Node -> NodeLabel  -> Attribute
toShapeAttr mCentralNode nodeId nodeLabel = shape $ case mCentralNode of
    Just centralNodeId
        | nodeId == centralNodeId -> Box3D
    _   | isAppModule nodeLabel   -> BoxShape
        | otherwise               -> Ellipse

toLabelAttr :: Bool -> NodeLabel -> Attribute
toLabelAttr clusteringEnabled nodeLabel =
    Label . StrLabel . fromStrict $ moduleName nodeLabel <> optionalPkgName
  where
    optionalPkgName = case (clusteringEnabled, packageName nodeLabel) of
        (False, Just pkgName) -> "\n<" <> pkgName <> ">"
        _                     -> ""

toColorAttr :: NodeLabel -> Attribute
toColorAttr nodeLabel = fillColor $ fromMaybe White $ do
    pkg <- packageName nodeLabel
    lookup pkg
        [ ("_factories", LightBlue)
        , ("_share", LightCoral)
        , ("app-monolithic", LightCyan)
        , ("audience-builder", LightGoldenrodYellow)
        , ("chart-builder",  LightGray)
        , ("components", LightGreen)
        , ("dashboards", LightPink)
        , ("fullscreen-search", LightSalmon)
        , ("gwiq", LightSeaGreen)
        , ("products", LightSkyBlue)
        , ("query-builder", LightSlateGray)
        , ("settings", LightSteelBlue)
        , ("tv-elm", LightYellow)
        , ("tv-study", LightCyan)
        ]

data GeneratorParams = GeneratorParams
  { centralNode               :: Maybe Node
  , enableTransitiveReduction :: Bool
  , clusteringEnabled         :: Bool
  }
