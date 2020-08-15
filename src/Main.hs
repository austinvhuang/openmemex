{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Data.Time.Clock

import Options.Generic


data Entry = Entry {
    entryID :: Int, 
    date :: String,
    time :: String,
    content :: String
    } deriving (Show)

data Tag = Tag {
    foreignID :: Int, 
    tag :: String
    } deriving (Show)

data CommandLine w = CommandLine { 
    note :: w ::: String <?> "A note to yourself"
    , tags :: w ::: [String] <?> "Tags for the note"
    , reset :: w ::: Bool <?> "Wipe Database" 
    } deriving (Generic)

instance ParseRecord (CommandLine Wrapped)
instance Show (CommandLine Unwrapped)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

initDB :: IO ()
initDB = do
  conn <- open "note2self.db"
  execute_ conn "DROP TABLE IF EXISTS entries;"
  execute_ conn "DROP TABLE IF EXISTS tags;"
  execute_ conn "CREATE TABLE entries (entryID INTEGER PRIMARY KEY, date text, time text, content text);"
  execute_ conn "CREATE TABLE tags (entryID, tag text);"
  close conn

main :: IO ()
main = do
  initDB
  options <- unwrapRecord "Note2Self"
  print (options :: CommandLine Unwrapped)
