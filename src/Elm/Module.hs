{-# LANGUAGE OverloadedStrings #-}
module Elm.Module
    ( ModuleInfo
    , ModuleName
    , defaultImports
    , nameToText
    , getImports
    , getName
    , loadModuleInfo
    ) where

import qualified AST.Source as Source
import qualified Data.ByteString as BS
import qualified Data.Name as Elm
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Utf8 as Utf8
import qualified Parse.Module
import qualified Reporting.Error.Syntax as Syntax (Error, toReport)
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report

import Data.Bifunctor (bimap)
import Data.Set (Set)
import Data.Text (Text)

data ModuleInfo = ModuleInfo
    { _moduleInfo_name    :: ModuleName
    , _moduleInfo_imports :: [ModuleName]
    }

newtype ModuleName = ModuleName Text
    deriving (Eq, Ord, Show)

getName :: ModuleInfo -> ModuleName
getName = _moduleInfo_name

nameToText :: ModuleName -> Text
nameToText (ModuleName txt) = txt

getImports :: ModuleInfo -> [ModuleName]
getImports = _moduleInfo_imports

loadModuleInfo :: FilePath -> IO (Either String ModuleInfo)
loadModuleInfo elmFile = do
    sourceCode <- BS.readFile elmFile
    let parseResult = Parse.Module.fromByteString Parse.Module.Application sourceCode
    pure $ bimap (formatError sourceCode) extractInfo parseResult

extractInfo :: Source.Module -> ModuleInfo
extractInfo modul = ModuleInfo moduleName imports
  where
    moduleName = fromElmName $ Source.getName modul
    imports = fromElmName . Source.getImportName <$> Source._imports modul

fromElmName :: Elm.Name -> ModuleName
fromElmName = ModuleName . Text.pack . Utf8.toChars

formatError :: BS.ByteString -> Syntax.Error -> String
formatError sourceCode err = show $ Report._message report
  where
    source = Code.toSource sourceCode
    report = Syntax.toReport source err

-- based on https://package.elm-lang.org/packages/elm/core/1.0.2/
defaultImports :: Set Text
defaultImports = Set.fromList
    [ "Basics"
    , "List"
    , "Maybe"
    , "Result"
    , "String"
    , "Char"
    , "Tuple"
    , "Debug"
    , "Platform"
    , "Platform.Cmd"
    , "Platform.Sub"
    ]
