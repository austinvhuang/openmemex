{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative
import Data.Semigroup ((<>))
import Data.Time.Clock
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Options.Applicative

data Entry = Entry
  { entryID :: Int,
    date :: String,
    time :: String,
    content :: String
  }
  deriving (Show)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field

data Tag = Tag
  { foreignID :: Int,
    tag :: String
  }
  deriving (Show)

data CommandLine = CommandLine
  { note :: String,
    tags :: [String],
    resetDB :: Bool
  }
  deriving (Show)

commandLine :: Parser CommandLine
commandLine =
  CommandLine
    <$> strOption
      ( long "note"
          <> help "Note content"
      )
    <*> many
      ( strOption
          ( long "tag"
              <> help "Topic tag"
          )
      )
    <*> switch (long "reset" <> help "Reset databse")

optionsParser :: ParserInfo CommandLine
optionsParser =
  info
    (helper <*> commandLine)
    ( fullDesc <> progDesc "note2self"
        <> header
          "note2self - take notes for yourself"
    )

dbFile = "note2self.db"

initDB :: IO ()
initDB = do
  conn <- open dbFile
  execute_ conn "DROP TABLE IF EXISTS entries;"
  execute_ conn "DROP TABLE IF EXISTS tags;"
  execute_ conn "CREATE TABLE entries (entryID INTEGER PRIMARY KEY, date text, time text, content text);"
  execute_ conn "CREATE TABLE tags (entryID, tag text);"
  close conn

dump = do
  putStrLn "DB Contents"
  conn <- open dbFile
  r <- query_ conn "SELECT * from entries" :: IO [Entry]
  mapM_ print r
  close conn

main :: IO ()
main = do
  initDB
  options <- execParser optionsParser
  print (options :: CommandLine)
  if (resetDB options)
    then putStrLn "Resetting DB" >> initDB
    else pure ()
  dump
  pure ()
