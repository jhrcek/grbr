{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Server where

import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedFile)
import Data.GraphViz.Commands (quitWithoutGraphviz)
import Data.Text.Lazy (Text, pack)
import Graph.DotBuilder (GeneratorParams (..), generateModuleDepGraph,
                         generatePackageDepGraph, getNeighborhood)
import Network.HTTP.Types (notFound404)
import Web.Browser (openBrowser)
import Web.Scotty (ActionM, file, get, json, param, raw, rescue, scotty, status,
                   text)

import qualified Elm.DepGraph as DepGraph

main :: IO ()
main = do
  quitWithoutGraphviz "This tool requires graphviz for diagram generation.\n\
                      \See https://graphviz.gitlab.io/download/ for installation instructions."
  modDeps <- DepGraph.loadModuleDependencies
  let (nodeCount, edgeCount) = DepGraph.getNodeAndEdgeCounts modDeps
  putStrLn $ "Loaded dependency graph with " <> show nodeCount
         <> " nodes and " <> show edgeCount <> " edges"
  _ <- openBrowser "http://localhost:3000/"
  scotty 3000 $ do
      get "/" $
          raw $ LBS.fromStrict $(embedFile "client/dist/index.html")
      get "/elm.js" $
          raw $ LBS.fromStrict $(embedFile "client/dist/js/elm.js")
      get "/nodes" $
          json modDeps
      get "/modules" $ do
          params <- getGeneratorParams
          generateModuleDepGraph params modDeps >>= file
      get "/packages" $ do
          params <- getGeneratorParams
          generatePackageDepGraph params modDeps >>= file
      get "/modules/:nodeId" $ do
          params <- getGeneratorParams
          case centralNode params of
              Nothing -> notFound "You must provide node ID in the URL!"
              Just nodeId ->
                  case getNeighborhood nodeId modDeps of
                      Nothing -> notFound $
                          "This graph doesn't have node with ID " <> pack (show nodeId)
                          <> ". Valid node IDs are 0 - " <> pack (show (nodeCount - 1))
                      Just neighborhood -> generateModuleDepGraph params neighborhood >>= file

notFound :: Text -> ActionM ()
notFound errorText = do
    status notFound404
    text errorText

getGeneratorParams :: ActionM GeneratorParams
getGeneratorParams = GeneratorParams
    <$> getNodeIdParam
    <*> getTredParam
    <*> getClusterParam

getClusterParam :: ActionM Bool
getClusterParam = param "cluster" `rescue` (\_err -> pure False)

getTredParam :: ActionM Bool
getTredParam = param "tred" `rescue` (\_err -> pure True)

getNodeIdParam :: ActionM (Maybe Int)
getNodeIdParam = (Just <$> param "nodeId") `rescue` (\_err -> pure Nothing)
