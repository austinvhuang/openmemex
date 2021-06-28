{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend where

import Database.SQLite.Simple
import GHC.Int (Int64)
import System.Directory (copyFile, removeFile)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import DB hiding (Entry)

data Entry = Entry
  { entryID :: Maybe Int, -- only needs a value when reading
    date :: String,
    time :: String,
    content :: String
  }
  deriving (Show)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field

addEntry :: Entry -> IO Int64
addEntry Entry {..} = do
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO entries (date, time, content) VALUES (:date, :time, :content)"
    [":date" := date, ":time" := time, ":content" := content]
  r <- lastInsertRowId conn
  close conn
  pure r

addTag :: Int64 -> String -> IO Int64
addTag entryID tag = do
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO tags (entry_id, tag) VALUES (:entryID, :tag)"
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
