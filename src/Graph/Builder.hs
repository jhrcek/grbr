{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph.Builder
    ( generateWholeGraph
    , generateNodeContext
    , getNeighborhood
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.Inductive.Graph (Node, gelem, neighbors, subgraph)
import Data.GraphViz (GraphvizParams, defaultParams, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Attributes (fillColor, filled, shape, style)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (..))
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir, URL),
                                          Label (StrLabel), RankDir (FromLeft),
                                          Shape (Box3D, BoxShape, Ellipse))
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised (GlobalAttributes (GraphAttrs))
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (fromStrict, pack)
import Graph.Types (ClusterLabel, DepGraph, EdgeLabel, ModuleDependencies (..),
                    NodeLabel, isAppModule, moduleName, packageName)

generateWholeGraph :: MonadIO io => ModuleDependencies -> io FilePath
generateWholeGraph ModuleDependencies{depGraph} =
    generateGraphFile (mkGeneratorParams Nothing True) depGraph

generateNodeContext :: MonadIO io => Node ->  ModuleDependencies -> io FilePath
generateNodeContext nodeId ModuleDependencies{depGraph} =
    generateGraphFile (mkGeneratorParams (Just nodeId) True) depGraph

generateGraphFile :: MonadIO io => GeneratorParams -> DepGraph -> io FilePath
generateGraphFile genParams g =
     liftIO $ runGraphviz dotGraph Svg "graph.svg"
   where
     gvParams = graphVizParams genParams
     dotGraph = applyTred $ graphToDot gvParams g
     applyTred = if enableTransitiveReduction genParams then transitiveReduction else id

getNeighborhood :: Node -> ModuleDependencies -> Maybe ModuleDependencies
getNeighborhood nodeId md@(ModuleDependencies{depGraph})
    | gelem nodeId depGraph = Just $ md { depGraph = subgraph (nodeId : neighbors depGraph nodeId) depGraph }
    | otherwise = Nothing

type GvParams = GraphvizParams Node NodeLabel EdgeLabel ClusterLabel NodeLabel

graphVizParams :: GeneratorParams -> GvParams
graphVizParams GeneratorParams{centralNode} = defaultParams
    { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
    , fmtNode = \(nodeId, nodeLabel) ->
        [ URL $ "/node/" <> pack (show nodeId)
        , toLabelAttr nodeLabel
        , toColorAttr nodeLabel
        , toShapeAttr centralNode nodeId nodeLabel
        , style filled
        ]
    }

toShapeAttr :: Maybe Node -> Node -> NodeLabel  -> Attribute
toShapeAttr mCentralNode nodeId nodeLabel = shape $ case mCentralNode of
    Just centralNodeId
        | nodeId == centralNodeId -> Box3D
    _   | isAppModule nodeLabel   -> BoxShape
        | otherwise               -> Ellipse

toLabelAttr :: NodeLabel -> Attribute
toLabelAttr nodeLabel =
    Label . StrLabel . fromStrict $ moduleName nodeLabel <> optionalPkgName
  where
    optionalPkgName = case packageName nodeLabel of
        Nothing      -> ""
        Just pkgName -> "\n<" <> pkgName <> ">"

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
  }

mkGeneratorParams :: Maybe Node -> Bool -> GeneratorParams
mkGeneratorParams = GeneratorParams
