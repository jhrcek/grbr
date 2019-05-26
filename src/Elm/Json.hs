{-# LANGUAGE OverloadedStrings #-}
module Elm.Json
  ( ElmJson
  , load
  , sourceDirectories, Error(..)
  ) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict, parseJSON, withObject,
                   (.:))
import Data.Bifunctor (first)
import Data.Text
import Prelude hiding (FilePath)
import Turtle

newtype ElmJson = ElmJson
    { sourceDirectories :: [FilePath]
    }

data Error
    = ElmJsonDoesNotExist FilePath
    | ElmJsonDecodeError String

instance FromJSON ElmJson where
  parseJSON = withObject "elm.json" $ \obj ->
      ElmJson . fmap fromText <$> obj .: "source-directories"

load :: FilePath -> IO (Either Error ElmJson)
load elmJson = do
    exists <- testfile elmJson
    if exists
        then first ElmJsonDecodeError <$> eitherDecodeFileStrict (Data.Text.unpack $ format fp elmJson)
        else pure . Left $ ElmJsonDoesNotExist elmJson
