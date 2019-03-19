module Graph.Types
  ( ModuleDependencies(..)
  , NodeLabel
  , EdgeLabel
  , ClusterLabel
  , DepGraph
  , labelText
  , isAppModule
  , mkNodeLabel
  ) where

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)

data ModuleDependencies = ModuleDependencies
    { depGraph   :: DepGraph
    }

type DepGraph = Gr NodeLabel EdgeLabel

data NodeLabel = NodeLabel
    { labelText   :: Text
    -- True for Modules belonging to the analyzed app, False for modules from dependencies
    , isAppModule :: Bool
    }

mkNodeLabel :: Text -> Bool -> NodeLabel
mkNodeLabel = NodeLabel

type EdgeLabel = ()

type ClusterLabel = ()
