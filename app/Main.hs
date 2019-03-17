{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.Inductive.Graph (Node, gelem, neighbors, subgraph)
import Data.GraphViz (GraphvizParams, defaultParams, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised
import Data.Text.Lazy (fromStrict, pack)
import Elm.Analyse (loadModuleDependencies)
import Graph (ClusterLabel, EdgeLabel, MyGraph, NodeLabel)
import Web.Scotty

main :: IO ()
main = do
  modDepsGraph <- loadModuleDependencies "a.json"
  scotty 3000 $ do
      get "/" $
          generateGraphFile modDepsGraph >>= file

      get "/node/:nodeId" $ do
          nodeId :: Int <- param "nodeId"
          case getNeighborhood nodeId modDepsGraph of
              Nothing -> text $ "This graph doesn't have node with id " <> pack (show nodeId)
              Just neighborhood -> generateGraphFile neighborhood >>= file


graphVizParams :: GraphvizParams Node NodeLabel EdgeLabel ClusterLabel NodeLabel
graphVizParams = defaultParams
    { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
    , fmtNode = \(nodeId, nodeLabel) ->
        [ shape BoxShape
        , URL $ "/node/" <> pack (show nodeId)
        , Label . StrLabel $ fromStrict nodeLabel
        ]
    }

generateGraphFile :: MonadIO io => MyGraph -> io FilePath
generateGraphFile g =
     liftIO $ runGraphviz dotGraph Svg "graph.svg"
   where
     dotGraph = graphToDot graphVizParams g

getNeighborhood :: Node -> MyGraph -> Maybe MyGraph
getNeighborhood nodeId gr
    | gelem nodeId gr = Just $ subgraph (nodeId : neighbors gr nodeId) gr
    | otherwise = Nothing
