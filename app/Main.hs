{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.Inductive.Graph (Node, gelem, neighbors, order, subgraph)
import Data.GraphViz (GraphvizParams, defaultParams, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Attributes (fillColor, filled, shape, style)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (LightSalmon))
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised
import qualified Data.IntSet as ISet
import Data.Text.Lazy (fromStrict, pack)
import Elm.Analyse (loadModuleDependencies)
import Graph (ClusterLabel, EdgeLabel, MyGraph, NodeLabel)
import Web.Scotty

main :: IO ()
main = do
  modDepsGraph <- loadModuleDependencies "nss.json"
  scotty 3000 $ do
      get "/" $
          generateWholeGraph modDepsGraph >>= file
      get "/node/:nodeId" $ do
          nodeId <- param "nodeId"
          case getNeighborhood nodeId modDepsGraph of
              Nothing -> text $ "This graph doesn't have node with ID " <> pack (show nodeId)
                             <> ". Valid node IDs are 0 - " <> pack (show (order modDepsGraph - 1))
              Just neighborhood -> generateNodeContext nodeId neighborhood >>= file

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
      highlight = ISet.fromList nodesToHighlight
      shouldHighlight nodeId = ISet.member nodeId highlight
      addFillColor nodeId
          | shouldHighlight nodeId = [style filled, fillColor LightSalmon]
          | otherwise              = []

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
