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

initDB :: IO ()
initDB = do
  copyFile dbFile (dbFile ++ ".backup")
  removeFile dbFile
  conn <- open dbFile
  bracketExecute "DROP TABLE IF EXISTS entries;"
  bracketExecute "DROP TABLE IF EXISTS tags;"
  bracketExecute "DROP TABLE IF EXISTS cache_meta;"
  bracketExecute "DROP TABLE IF EXISTS annotations;"
  bracketExecute "CREATE TABLE entries (entry_id INTEGER PRIMARY KEY AUTOINCREMENT, date TEXT, time TEXT, content TEXT);"
  bracketExecute "CREATE TABLE tags (tag_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, tag TEXT);"
  bracketExecute "CREATE TABLE cache_meta (cache_table_id INTEGER PRIMARY KEY AUTOINCREMENT, table_name TEXT, cache_date TEXT, cache_time TEXT);"
  bracketExecute "CREATE TABLE annotations(annotation_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, annotation_date TEXT, annotation_time TEXT, annotation_content TEXT);"
  bracketExecute "CREATE INDEX idx_tags_entry_id ON tags(entry_id);"
  bracketExecute "CREATE INDEX idx_entries_time on entries(time);"
  bracketExecute "CREATE INDEX idx_entries_date on entries(date);"
  bracketExecute "CREATE INDEX idx_tags_tag ON tags(tag);"
  bracketExecute "CREATE UNIQUE INDEX idx_entries_entry_id ON entries(entry_id);"
  bracketExecute "CREATE INDEX idx_annotations_date on annotations(annotation_date);"
  bracketExecute "CREATE INDEX idx_annotations_time on annotations(annotation_time);"
  bracketExecute "CREATE INDEX idx_annotations_entry_id on annotations(entry_id);"
  now <- getZonedTime
  let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
      tm = formatTime defaultTimeLocale "%H:%M:%S" now
      timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      tableName = "cache_" ++ timeStamp
  bracketExecute $ "CREATE TABLE " ++ tableName
              ++ "(cache_entry_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, "
              ++ "cache_url TEXT, "
              ++ "cache_content_type TEXT, cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT, cache_ocr_file TEXT);"
  bracketExecute $ "DROP TABLE IF EXISTS " ++ tableName ++ ";"
  bracketExecute "DROP VIEW IF EXISTS cache;"
  bracketExecute $
        "CREATE VIEW IF NOT EXISTS cache(cache_entry_id, entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content) "
          ++ "as select cache_entry_id, "
          ++ "entries.entry_id as entry_id, cache_url, cache_content_type, coalesce(cache_title, entries.content), cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content "
          ++ "from entries"
          ++ " left join "
          ++ tableName
          ++ " on "
          ++ tableName
          ++ ".entry_id=entries.entry_id;"

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
