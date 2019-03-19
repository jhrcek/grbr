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
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (LightBlue, LightSalmon))
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir, URL),
                                          Label (StrLabel), RankDir (FromLeft),
                                          Shape (BoxShape))
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised (GlobalAttributes (GraphAttrs))
import Data.Text.Lazy (fromStrict, pack)
import Graph.Types (ClusterLabel, DepGraph, EdgeLabel, ModuleDependencies (..),
                    NodeLabel, isAppModule, labelText)


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
        highlightNode nodeId nodeLabel ++
        [ shape BoxShape
        , URL $ "/node/" <> pack (show nodeId)
        , Label . StrLabel . fromStrict $ labelText nodeLabel
        ]
    }
    where
      highlightNode nodeId nodeLabel = case centralNode of
          Just centralNodeId
              | nodeId == centralNodeId -> [style filled, fillColor LightSalmon]
          _   | isAppModule nodeLabel   -> [style filled, fillColor LightBlue]
              | otherwise               -> []


data GeneratorParams = GeneratorParams
  { centralNode               :: Maybe Node
  , enableTransitiveReduction :: Bool
  }

mkGeneratorParams :: Maybe Node -> Bool -> GeneratorParams
mkGeneratorParams = GeneratorParams
