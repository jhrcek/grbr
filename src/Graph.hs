{-# LANGUAGE OverloadedStrings #-}

module Graph
  ( MyGraph
  , NodeLabel
  , EdgeLabel
  , ClusterLabel
  , example
  ) where

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text

type MyGraph = Gr NodeLabel EdgeLabel
type NodeLabel = Text
type EdgeLabel = ()
type ClusterLabel = ()

example :: MyGraph
example = mkGraph
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
