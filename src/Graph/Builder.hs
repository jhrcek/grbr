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
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (LightSalmon))
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir, URL),
                                          Label (StrLabel), RankDir (FromLeft),
                                          Shape (BoxShape))
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised (GlobalAttributes (GraphAttrs))
import qualified Data.IntSet as Set
import Data.Text.Lazy (fromStrict, pack)
import Graph (ClusterLabel, EdgeLabel, MyGraph, NodeLabel)

generateWholeGraph :: MonadIO io => MyGraph -> io FilePath
generateWholeGraph =
    generateGraphFile (graphVizParams [])

generateNodeContext :: MonadIO io => Node ->  MyGraph -> io FilePath
generateNodeContext nodeId =
    generateGraphFile (graphVizParams [nodeId])

generateGraphFile :: MonadIO io => MyGraphvizParams -> MyGraph -> io FilePath
generateGraphFile gvParams g =
     liftIO $ runGraphviz dotGraph Svg "graph.svg"
   where
     dotGraph = transitiveReduction {-TODO make tred optional-}$ graphToDot gvParams g

getNeighborhood :: Node -> MyGraph -> Maybe MyGraph
getNeighborhood nodeId gr
    | gelem nodeId gr = Just $ subgraph (nodeId : neighbors gr nodeId) gr
    | otherwise = Nothing

type MyGraphvizParams = GraphvizParams Node NodeLabel EdgeLabel ClusterLabel NodeLabel

graphVizParams :: [Node] -> MyGraphvizParams
graphVizParams nodesToHighlight = defaultParams
    { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
    , fmtNode = \(nodeId, nodeLabel) ->
        addFillColor nodeId ++
        [ shape BoxShape
        , URL $ "/node/" <> pack (show nodeId)
        , Label . StrLabel $ fromStrict nodeLabel
        ]
    }
    where
      highlight = Set.fromList nodesToHighlight
      shouldHighlight nodeId = Set.member nodeId highlight
      addFillColor nodeId
          | shouldHighlight nodeId = [style filled, fillColor LightSalmon]
          | otherwise              = []
