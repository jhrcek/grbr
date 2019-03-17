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
   <> progDesc "Print a greeting for TARGET"
   <> header "hello - a test for optparse-applicative" )

sample :: Parser Options
sample = Options
  <$> strOption
      ( long "infile"
     <> metavar "FILE"
     <> help "File that is the result of 'elm-analyse --format json'" )
