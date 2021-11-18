{-# LANGUAGE DeriveGeneric #-}

module ArgParser where

import Control.Applicative
import Data.Semigroup ((<>))
import Options.Applicative
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics(Generic)

data Configuration = Configuration
  { showTagThresh :: Int,
    resultsPerPage :: Int,
    port :: Int,
    dbFilename :: String
  } deriving (Show, Generic)

instance ToJSON Configuration
instance FromJSON Configuration

commandLine :: Parser Configuration
commandLine =
  Configuration
    <$> option auto
      ( long "tag_thresh"
          <> help "Threshold for minimum number of memex entries of a tag to display it in the gallery."
          <> showDefault
          <> value 1
          <> metavar "INT"
      )
    <*> option auto
      ( long "results_per_page"
          <> help "Maximum number of results to show on a page."
          <> showDefault
          <> value 150
          <> metavar "INT"
      )

    <*> option auto
      ( long "port"
          <> help "Server port."
          <> showDefault
          <> value 3000
          <> metavar "INT"
      )
    <*> strOption
          ( long "dbfile"
         <> help "SQLite database file." 
          <> metavar "FILENAME"
         <> showDefault
         <> value "openmemex.db"
         )


optionsParser :: ParserInfo Configuration
optionsParser =
  info
    (helper <*> commandLine)
        (header "OpenMemex: An open source, local-first knowledge platform.")
          
