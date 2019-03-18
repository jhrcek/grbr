module Options
    ( Options(Options, inputFile)
    , parse
    ) where

import Options.Applicative

newtype Options = Options
    { inputFile :: FilePath
    }

parse :: IO Options
parse = execParser $ info (sample <**> helper)
    ( fullDesc
   <> progDesc "Browser of module dependency graph of Elm projects"
   <> header "Graph Browser" )

sample :: Parser Options
sample = Options
  <$> strArgument
      ( metavar "FILE"
     <> help "File which is the result of 'elm-analyse --format json'" )
