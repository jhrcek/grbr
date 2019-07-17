{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph
    ( ModuleDependencies(..)
    , NodeLabel
    , EdgeLabel
    , ClusterLabel
    , DepGraph
    , Package(..)
    , moduleName
    , packageName
    , isAppModule
    , mkNodeLabel
    , getNodeAndEdgeCounts
    ) where

import qualified Data.Vector as Vector

import Data.Aeson (ToJSON, Value (Array), object, toJSON, (.=))
import Data.Graph.Inductive.Graph (order, size)
import Data.Graph.Inductive.Graph (labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)

newtype ModuleDependencies = ModuleDependencies
    { depGraph :: DepGraph
    }

type DepGraph = Gr NodeLabel EdgeLabel

data NodeLabel = NodeLabel
    { moduleName  :: Text --e.g. "Html.Attributes"
    , packageName :: Maybe Package
    -- True for Modules belonging to the analyzed app, False for modules from dependencies
    , isAppModule :: Bool
    }

type EdgeLabel = ()

type ClusterLabel = Package

-- | Represents elm package name
newtype Package = Package Text
    deriving (Eq, Ord, Show, ToJSON) via Text


mkNodeLabel :: Text ->  Maybe Package -> Bool -> NodeLabel
mkNodeLabel = NodeLabel

instance ToJSON ModuleDependencies where
    toJSON (ModuleDependencies depGraph_) =
      Array
      . Vector.fromList
      . fmap (\(nodeId, nodeLabel) -> object
          [ "id" .= nodeId
          , "label" .= moduleName nodeLabel
          , "group" .= packageName nodeLabel
          ] )
      $ labNodes depGraph_

getNodeAndEdgeCounts :: ModuleDependencies -> (Int, Int)
getNodeAndEdgeCounts ModuleDependencies{depGraph} =
    (order depGraph, size depGraph)
