{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}

module Backend where

import GHC.Int (Int64)

import System.Directory (copyFile)

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Entry = Entry
  { entryID :: Maybe Int, -- only needs a value when reading
    date :: String,
    time :: String,
    content :: String
  }
  deriving (Show)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field

data Tag = Tag
  { 
    tagID :: Maybe Int, -- only needs a value when reading
    foreignID :: Int,
    tag :: String
  }
  deriving (Show)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

dbFile = "note2self.db"

initDB :: IO ()
initDB = do
  copyFile dbFile (dbFile ++ ".backup")
  conn <- open dbFile
  execute_ conn "DROP TABLE IF EXISTS entries;"
  execute_ conn "DROP TABLE IF EXISTS tags;"
  execute_ conn "CREATE TABLE entries (entryID INTEGER PRIMARY KEY AUTOINCREMENT, date TEXT, time TEXT, content TEXT);"
  execute_ conn "CREATE TABLE tags (tagID INTEGER PRIMARY KEY AUTOINCREMENT, entryID INTEGER, tag TEXT);"
  close conn

addEntry :: Entry -> IO Int64
addEntry Entry{..} = do
  conn <- open dbFile
  executeNamed conn 
    "INSERT INTO entries (date, time, content) VALUES (:date, :time, :content)" 
    [":date" := date, ":time" := time, ":content" := content]
  r <- lastInsertRowId conn
  close conn
  pure r

addTag :: Int64 -> String -> IO Int64
addTag entryID tag = do
  conn <- open dbFile
  executeNamed conn 
    "INSERT INTO tags (entryID, tag) VALUES (:entryID, :tag)" 
    [":entryID" := entryID, ":tag" := tag]
  r <- lastInsertRowId conn
  close conn
  pure r

dumpEntries = do
  putStrLn "Entries ---"
  conn <- open dbFile
  r <- query_ conn "SELECT * from entries" :: IO [Entry]
  mapM_ print r
  close conn

dumpTags = do
  putStrLn "Tags ---"
  conn <- open dbFile
  r <- query_ conn "SELECT * from tags" :: IO [Tag]
  mapM_ print r
  close conn
