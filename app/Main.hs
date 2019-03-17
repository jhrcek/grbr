{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.Inductive.Graph (Node, gelem, neighbors, subgraph)
import Data.GraphViz (GraphvizParams, defaultParams, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Attributes (shape, fillColor, style, filled)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised
import Data.Text.Lazy (fromStrict, pack)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor(LightSalmon))
import Elm.Analyse (loadModuleDependencies)
import Graph (ClusterLabel, EdgeLabel, MyGraph, NodeLabel)
import Web.Scotty
import qualified Data.IntSet as ISet

main :: IO ()
main = do
  modDepsGraph <- loadModuleDependencies "a.json"
  scotty 3000 $ do
      get "/" $
          generateWholeGraph modDepsGraph >>= file

      get "/node/:nodeId" $ do
          nodeId :: Int <- param "nodeId"
          case getNeighborhood nodeId modDepsGraph of
              Nothing -> text $ "This graph doesn't have node with id " <> pack (show nodeId)
              Just neighborhood -> generateGraphFileWithHighlightedNode nodeId neighborhood >>= file


type MyGraphvizParams = GraphvizParams Node NodeLabel EdgeLabel ClusterLabel NodeLabel

graphVizParams :: [Node] -> MyGraphvizParams
graphVizParams nodesToHighlight = defaultParams
    { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
    , fmtNode = \(nodeId, nodeLabel) ->
        (if shouldHighlight nodeId then ([style filled, fillColor LightSalmon]++) else id)
        [ shape BoxShape
        , URL $ "/node/" <> pack (show nodeId)
        , Label . StrLabel $ fromStrict nodeLabel
        ]
    }
    where
      highlight = ISet.fromList nodesToHighlight
      shouldHighlight nodeId = ISet.member nodeId highlight

generateWholeGraph :: MonadIO io => MyGraph -> io FilePath
generateWholeGraph =
    generateGraphFile (graphVizParams [])

generateGraphFileWithHighlightedNode :: MonadIO io => Node ->  MyGraph -> io FilePath
generateGraphFileWithHighlightedNode nodeId =
    generateGraphFile (graphVizParams [nodeId])


generateGraphFile :: MonadIO io => MyGraphvizParams -> MyGraph -> io FilePath
generateGraphFile gvParams g =
     liftIO $ runGraphviz dotGraph Svg "graph.svg"
   where
     dotGraph = graphToDot gvParams g

getNeighborhood :: Node -> MyGraph -> Maybe MyGraph
getNeighborhood nodeId gr
    | gelem nodeId gr = Just $ subgraph (nodeId : neighbors gr nodeId) gr
    | otherwise = Nothing
