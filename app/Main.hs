{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Graph.Inductive.Graph (order)
import Data.Text.Lazy (pack)
import Elm.Analyse (loadModuleDependencies)
import Graph.Builder (generateNodeContext, generateWholeGraph, getNeighborhood)
import Options (Options (..))
import qualified Options
import Web.Browser (openBrowser)
import Web.Scotty (file, get, param, scotty, text)

main :: IO ()
main = do
  Options{inputFile} <- Options.parse
  modDepsGraph <- loadModuleDependencies inputFile
  _ <- openBrowser "http://localhost:3000"
  scotty 3000 $ do
      get "/" $
          generateWholeGraph modDepsGraph >>= file
      get "/node/:nodeId" $ do
          nodeId <- param "nodeId"
          case getNeighborhood nodeId modDepsGraph of
              Nothing -> text $ "This graph doesn't have node with ID " <> pack (show nodeId)
                             <> ". Valid node IDs are 0 - " <> pack (show (order modDepsGraph - 1))
              Just neighborhood -> generateNodeContext nodeId neighborhood >>= file
