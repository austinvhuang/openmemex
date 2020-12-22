module Main where

import Data.Time

import Options.Applicative

import Backend
import Parser

main :: IO ()
main = do
  options <- execParser optionsParser
  print (options :: CommandLine)
  if (resetDB options)
    then putStrLn "Resetting DB" >> initDB >> putStrLn "Previous DB saved as note2self.db.backup"
    else pure ()

  now <- getZonedTime
  let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
  let tm = formatTime defaultTimeLocale "%H:%M:%S" now
  r <- addEntry $ Entry Nothing dt tm (note options)
  mapM (addTag r) (tags options)
  dumpEntries
  dumpTags
  pure ()
