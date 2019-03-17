{-# LANGUAGE OverloadedStrings #-}

module Graph
  ( MyGraph
  , NodeLabel
  , EdgeLabel
  , ClusterLabel
  ) where

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)

type MyGraph = Gr NodeLabel EdgeLabel

type NodeLabel = Text

type EdgeLabel = ()

type ClusterLabel = ()
