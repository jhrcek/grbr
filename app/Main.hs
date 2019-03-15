{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Graph.Inductive.Graph (Node, gelem, mkGraph, neighbors, subgraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (GraphvizParams, defaultParams, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types.Generalised
import Data.Text.Lazy (pack)
import Web.Scotty

main :: IO ()
main =
  scotty 3000 $ do
      get "/" $
          generateGraphFile g0 >>= file

      get "/node/:nodeId" $ do
          nodeId :: Int <- param "nodeId"
          case getNeighborhood nodeId g0 of
              Nothing -> text $ "This graph doesn't have node with id " <> pack (show nodeId)
              Just neighborhood -> generateGraphFile neighborhood >>= file


graphVizParams :: GraphvizParams Node NodeLabel EdgeLabel ClusterLabel String
graphVizParams = defaultParams
    { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
    , fmtNode = \(nodeId, nodeLabel) ->
        [ shape BoxShape
        , URL $ "/node/" <> pack (show nodeId)
        , Label . StrLabel $ pack nodeLabel
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

type MyGraph = Gr NodeLabel EdgeLabel
type NodeLabel = String
type EdgeLabel = ()
type ClusterLabel = ()

g0 :: MyGraph
g0 = mkGraph
    [ (1, "hello")
    , (2, "how")
    , (3, "are")
    , (4, "you")
    , (5, "there")
    ]
    [ (1,2,())
    , (2,3,())
    , (3,4,())
    , (3,5,())
    ]
