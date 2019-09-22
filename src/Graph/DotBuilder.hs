{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph.DotBuilder
    ( GeneratorParams (..)
    , generateModuleDepGraph
    , getNeighborhood
    , generatePackageDepGraph
    ) where

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Gwi

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Graph.Inductive.Graph (Edge, LEdge, LNode, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (GraphvizParams, NodeCluster (C, N), clusterBy, clusterID,
                      defaultParams, fmtCluster, fmtNode, globalAttributes,
                      graphToDot)
import Data.GraphViz.Algorithms (transitiveReduction)
import Data.GraphViz.Attributes (fillColor, filled, shape, style)
import Data.GraphViz.Attributes.Colors.SVG (SVGColor (..))
import Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir, URL),
                                          Label (StrLabel), RankDir (FromLeft),
                                          Shape (Box3D, BoxShape, Ellipse))
import Data.GraphViz.Commands (GraphvizOutput (Svg), runGraphviz)
import Data.GraphViz.Types (PrintDotRepr)
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.GraphViz.Types.Generalised (GlobalAttributes (GraphAttrs),
                                        GraphID (Str))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, pack)
import Graph (ClusterLabel, EdgeLabel, ModuleDependencies (..), NodeLabel,
              Package (..), isAppModule, moduleName, packageName)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

generateModuleDepGraph :: MonadIO io => GeneratorParams -> ModuleDependencies -> io FilePath
generateModuleDepGraph genParams ModuleDependencies{depGraph} =
    generateGraphFile dotGraph
  where
    gvParams = graphVizParams genParams
    dotGraph = transitiveReductionWhen (enableTransitiveReduction genParams)
               $ graphToDot gvParams depGraph

generatePackageDepGraph :: MonadIO io => GeneratorParams -> ModuleDependencies -> io FilePath
generatePackageDepGraph genParams ModuleDependencies{depGraph} =
    generateGraphFile dotGraph
  where
    dotGraph :: DotGraph Node
    dotGraph = transitiveReductionWhen (enableTransitiveReduction genParams)
               $ graphToDot gvParams packageDepGraph

    gvParams :: GraphvizParams Node Package () () Package
    gvParams = defaultParams
        { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
        , fmtNode = \(_nodeId, p@(Package pkgName)) ->
            [ textLabel pkgName
            , fillColor . fromMaybe White . lookup p $ zip Gwi.knownPackages [ LightBlue .. ]
            , style filled
            , shape BoxShape
            ]
        }

    packageDepGraph :: Gr Package ()
    packageDepGraph = Graph.mkGraph (Set.toList nodes) (Set.toList edges)
      where
        (nodes, edges, _) = foldr processEdge (Set.empty, Set.empty, Map.empty) $ Graph.edges depGraph

    lookupOrCreateNewId :: Package -> Map Package Node -> (Node, Map Package Node)
    lookupOrCreateNewId pkg m = case Map.lookup pkg m of
        Just existingId -> (existingId, m)
        Nothing -> let newId = Map.size m
                   in (newId, Map.insert pkg newId m)

    lookupPkgLabel :: Node -> Maybe Package
    lookupPkgLabel = Graph.lab depGraph >=> packageName

    processEdge :: Edge
                   -> (Set (LNode Package), Set (LEdge ()), Map Package Node)
                   -> (Set (LNode Package), Set (LEdge ()), Map Package Node)
    processEdge (from, to) acc =
        -- We're ignoring all modules the package of which we don't know
        fromMaybe acc $
            liftA2 (processPackages acc)
                (lookupPkgLabel from)
                (lookupPkgLabel to)
      where
        processPackages :: (Set (LNode Package), Set (LEdge ()), Map Package Node)
                           -> Package
                           -> Package
                           -> (Set (LNode Package), Set (LEdge ()), Map Package Node)
        processPackages (ns, es, m0) pkgFrom pkgTo =
           -- Originally I accumulated the data into graph, but repeatedly
           -- inserting the same nodes leads to its associated edges being removed (!?)
           -- So I'm accumulating to 2 sets instead.
           ( ns & Set.insert (fromId, pkgFrom)
                & Set.insert (toId, pkgTo)
           , es & Set.insert (fromId, toId, ())
           , m2
           )
          where
            (fromId, m1) = lookupOrCreateNewId pkgFrom m0
            (toId, m2) = lookupOrCreateNewId pkgTo m1



generateGraphFile :: (MonadIO io, PrintDotRepr g n) => g n -> io FilePath
generateGraphFile dotGraph = liftIO $ do
    tmpDir <- getTemporaryDirectory
    runGraphviz dotGraph Svg (tmpDir </> "graph.svg")

getNeighborhood :: Node -> ModuleDependencies -> Maybe ModuleDependencies
getNeighborhood nodeId md@ModuleDependencies{depGraph}
    | Graph.gelem nodeId depGraph = Just $ md { depGraph = Graph.subgraph (nodeId : Graph.neighbors depGraph nodeId) depGraph }
    | otherwise = Nothing

type ModuleGraphParams = GraphvizParams Node NodeLabel EdgeLabel ClusterLabel NodeLabel

graphVizParams :: GeneratorParams -> ModuleGraphParams
graphVizParams GeneratorParams{centralNode, clusteringEnabled, enableTransitiveReduction}
    | clusteringEnabled = customizedParams
        { clusterBy = clusterByPackage
        , clusterID = \(Package pkgName) -> Str (fromStrict pkgName)
        , fmtCluster = \(Package pkgName) -> [ GraphAttrs [ textLabel pkgName ]]
        }
    | otherwise = customizedParams
  where
    customizedParams = defaultParams
        { globalAttributes = [ GraphAttrs [RankDir FromLeft] ]
        , fmtNode = \(nodeId, nodeLabel) ->
            [ URL $ "/modules/"    <> pack (show nodeId)
                  <> "?cluster=" <> bool "false" "true" clusteringEnabled
                  <> "&amp;tred=" <> bool "false" "true" enableTransitiveReduction
            , toLabelAttr clusteringEnabled nodeLabel
            , toColorAttr nodeLabel
            , toShapeAttr centralNode nodeId nodeLabel
            , style filled
            ]
        }

transitiveReductionWhen :: Ord n => Bool -> DotGraph n -> DotGraph n
transitiveReductionWhen = bool id transitiveReduction

clusterByPackage :: (Node, NodeLabel)  -> NodeCluster ClusterLabel (Node, NodeLabel)
clusterByPackage pair@(_, nodeLabel) = case packageName nodeLabel of
    Nothing      -> N pair
    Just pkgName -> C pkgName $ N pair

toShapeAttr :: Maybe Node -> Node -> NodeLabel  -> Attribute
toShapeAttr mCentralNode nodeId nodeLabel = shape $ case mCentralNode of
    Just centralNodeId
        | nodeId == centralNodeId -> Box3D
    _   | isAppModule nodeLabel   -> BoxShape
        | otherwise               -> Ellipse

toLabelAttr :: Bool -> NodeLabel -> Attribute
toLabelAttr clusteringEnabled nodeLabel =
    textLabel $ moduleName nodeLabel <> optionalPkgName
  where
    optionalPkgName = case (clusteringEnabled, packageName nodeLabel) of
        (False, Just (Package pkgName)) -> "\n<" <> pkgName <> ">"
        _                               -> ""

textLabel :: Text -> Attribute
textLabel = Label . StrLabel . fromStrict

toColorAttr :: NodeLabel -> Attribute
toColorAttr nodeLabel = fillColor $ fromMaybe White $ do
    pkg <- packageName nodeLabel
    lookup pkg $ zip Gwi.knownPackages [ LightBlue .. ]

data GeneratorParams = GeneratorParams
  { centralNode               :: Maybe Node
  , enableTransitiveReduction :: Bool
  , clusteringEnabled         :: Bool
  }
