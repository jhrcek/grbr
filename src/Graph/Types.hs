{-# LANGUAGE OverloadedStrings #-}
module Graph.Types
  ( ModuleDependencies(..)
  , NodeLabel
  , EdgeLabel
  , ClusterLabel
  , DepGraph
  , moduleName
  , packageName
  , isAppModule
  , mkNodeLabel
  ) where

import Data.Aeson (ToJSON, Value (Array), object, toJSON, (.=))
import Data.Graph.Inductive.Graph (labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)
import qualified Data.Vector as Vector

newtype ModuleDependencies = ModuleDependencies
    { depGraph   :: DepGraph
    }

type DepGraph = Gr NodeLabel EdgeLabel

data NodeLabel = NodeLabel
    { moduleName  :: Text --e.g. "Html.Attributes"
    , packageName :: Maybe Text
    -- True for Modules belonging to the analyzed app, False for modules from dependencies
    , isAppModule :: Bool
    }

mkNodeLabel :: Text ->  Maybe Text -> Bool -> NodeLabel
mkNodeLabel = NodeLabel

type EdgeLabel = ()

type ClusterLabel = Text

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
