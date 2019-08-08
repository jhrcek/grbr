module Main where

import qualified GHC.IO.Encoding as Encoding
import qualified Server

main :: IO ()
main = do
    -- nix build causes localeEncoding to be ASCII, which causes failures when reading elm files
    Encoding.setLocaleEncoding Encoding.utf8
    Server.main
