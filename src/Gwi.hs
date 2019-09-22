{-# LANGUAGE OverloadedStrings #-}
module Gwi (knownPackages) where

import Data.Coerce (coerce)
import Data.Text (Text)
import Graph (Package(..))

knownPackages :: [Package]
knownPackages = coerce
    [ "_factories" :: Text
    , "_share"
    , "app-monolithic"
    , "audience-builder"
    , "chart-builder"
    , "components"
    , "dashboards"
    , "fullscreen-search"
    , "gwiq"
    , "products"
    , "crosstab-builder"
    , "settings"
    , "tv-elm"
    , "tv-study"
    ]
