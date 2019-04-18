{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import Data.Text.Lazy (pack)
import Elm.Analyse (getNodeAndEdgeCounts, loadModuleDependencies)
import Graph.Builder (GeneratorParams (..), generateGraphFile, getNeighborhood)
import Options (Options (..))
import qualified Options
import Web.Browser (openBrowser)
import Web.Scotty (ActionM, file, get, param, rescue, scotty, text, raw)
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)

main :: IO ()
main = do
  Options{inputFile} <- Options.parse
  modDeps <- loadModuleDependencies inputFile
  let (nodeCount, edgeCount) = getNodeAndEdgeCounts modDeps
  putStrLn $ "Loaded dependency graph with " <> show nodeCount
         <> " nodes and " <> show edgeCount <> " edges"
  _ <- openBrowser "http://localhost:3000/index.html"
  scotty 3000 $ do
      get "/index.html" $
        raw $ fromStrict $(embedFile "client/dist/index.html")
      get "/elm.js" $
        raw $ fromStrict $(embedFile "client/dist/js/elm.js")
      get "/" $ do
          params <- getGeneratorParams
          generateGraphFile params modDeps >>= file
      get "/node/:nodeId" $ do
          params <- getGeneratorParams
          case centralNode params of
              Nothing -> text "You must provide node ID in the URL!"
              Just nodeId ->
                  case getNeighborhood nodeId modDeps of
                      Nothing -> text $ "This graph doesn't have node with ID " <> pack (show nodeId)
                                     <> ". Valid node IDs are 0 - " <> pack (show (nodeCount - 1))
                      Just neighborhood -> generateGraphFile params neighborhood >>= file

getGeneratorParams :: ActionM GeneratorParams
getGeneratorParams = do
    mNodeId <- getNodeIdParam
    isCluster <- getClusterParam
    isTred <- getTredParam
    return $ GeneratorParams
        { centralNode = mNodeId
        , enableTransitiveReduction = isTred
        , clusteringEnabled = isCluster
        }

getClusterParam :: ActionM Bool
getClusterParam = param "cluster" `rescue` (\_err -> pure False)

getTredParam :: ActionM Bool
getTredParam = param "tred" `rescue` (\_err -> pure True)

getNodeIdParam :: ActionM (Maybe Int)
getNodeIdParam = (Just <$> param "nodeId") `rescue` (\_err -> pure Nothing)
