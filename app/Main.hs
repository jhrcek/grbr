{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import Data.GraphViz.Commands (quitWithoutGraphviz)
import Data.Text.Lazy (pack)
import Graph.DotBuilder (GeneratorParams (..), generateModuleDepGraph,
                         generatePackageDepGraph, getNeighborhood)
import Web.Browser (openBrowser)
import Web.Scotty (ActionM, file, get, json, param, raw, rescue, scotty, text)

import qualified Elm.DepGraph as DepGraph
import qualified GHC.IO.Encoding as Encoding

main :: IO ()
main = do
  print =<< Encoding.getLocaleEncoding
  print =<< Encoding.getFileSystemEncoding
  Encoding.setFileSystemEncoding Encoding.utf8
  Encoding.setLocaleEncoding Encoding.utf8

  quitWithoutGraphviz "This tool requires graphviz for diagram generation.\n\
                      \See https://graphviz.gitlab.io/download/ for installation instructions."
  modDeps <- DepGraph.loadModuleDependencies
  let (nodeCount, edgeCount) = DepGraph.getNodeAndEdgeCounts modDeps
  putStrLn $ "Loaded dependency graph with " <> show nodeCount
         <> " nodes and " <> show edgeCount <> " edges"
  _ <- openBrowser "http://localhost:3000/index.html"
  scotty 3000 $ do
      get "/index.html" $
          raw $ fromStrict $(embedFile "client/dist/index.html")
      get "/elm.js" $
          raw $ fromStrict $(embedFile "client/dist/js/elm.js")
      get "/nodes" $
          json modDeps
      get "/" $ do
          params <- getGeneratorParams
          generateModuleDepGraph params modDeps >>= file
      get "/packages" $ do
          params <- getGeneratorParams
          generatePackageDepGraph params modDeps >>= file
      get "/node/:nodeId" $ do
          params <- getGeneratorParams
          case centralNode params of
              Nothing -> text "You must provide node ID in the URL!"
              Just nodeId ->
                  case getNeighborhood nodeId modDeps of
                      Nothing -> text $ "This graph doesn't have node with ID " <> pack (show nodeId)
                                     <> ". Valid node IDs are 0 - " <> pack (show (nodeCount - 1))
                      Just neighborhood -> generateModuleDepGraph params neighborhood >>= file

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
