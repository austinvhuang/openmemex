{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

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
