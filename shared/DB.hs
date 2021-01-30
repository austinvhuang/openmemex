{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import System.Directory (copyFile)
import Text.Printf (printf)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix (takeBaseName)

data Date = Date
  { year :: String,
    month :: String,
    day :: String
  }
  deriving (Show, Generic)

-- Note entries

data Entry = Entry
  { entryID :: Int,
    date :: String,
    time :: String,
    content :: String
  }
  deriving (Eq, Show, Generic)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field

instance ToJSON Entry

-- tag entries

data Tag = Tag
  { tagID :: Maybe Int,
    foreignID :: Int,
    tag :: String
  }
  deriving (Show, Generic)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

instance ToJSON Tag

-- Cache Tables

data WebPage = WebPage {
  title :: String,
  body :: String
} deriving (Eq, Show, Generic)

data URLType = ArxivURL | TwitterURL | PdfURL | GenericURL

data CacheContentType = CachePage | CacheGenericContent deriving (Show, Generic)

-- API Service View
data CacheView = CacheView
  { cvForeignID :: Int, -- entryID
    cvUrl :: String,
    cvContentType :: String, -- CacheContentType,
    cvContent :: String,
    cvDate :: String,
    cvTime :: String
  }
  deriving (Show, Generic)

instance FromRow CacheView where
  fromRow = CacheView <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON CacheView

--  database representation
data CacheEntry = CacheEntry
  { cacheForeignID :: Int, -- entryID
    cacheUrl :: String,
    cacheContentType :: String, -- CacheContentType,
    cacheTitle :: String,
    cacheBody :: String,
    cacheScreenshotFile :: String,
    cacheOCRFile :: String
  }
  deriving (Show, Generic)

instance FromRow CacheEntry where
  fromRow = CacheEntry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON CacheEntry

data OCREntry = OCREntry
  { ocrForeignID :: Int, -- entryID
    ocrFile :: String,
    ocrContent :: String } deriving (Generic)

instance FromRow OCREntry where
  fromRow = OCREntry <$> field <*> field <*> field

dbFile = "note2self.db"

-- Helper functions

-- | date2string year month day
date2string :: Int -> Int -> Int -> String
date2string = printf "%.4d-%.2d-%.2d" 

-- Handlers

queryDate :: Int -> Int -> Int -> IO [Entry]
queryDate year month day = do
  conn <- open dbFile
  let queryString = Query $ pack $ "SELECT * FROM entries WHERE date == \"" ++ date2string year month day ++ "\""
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r

queryRange :: Int -> Int -> Int -> Int -> Int -> Int -> IO [Entry]
queryRange startYear startMonth startDay endYear endMonth endDay = do
  conn <- open dbFile
  let queryString =
        Query $
          pack $
            "SELECT * FROM entries WHERE date BETWEEN \""
              ++ date2string startYear startMonth startDay
              ++ "\" AND \""
              ++ date2string endYear endMonth endDay
              ++ "\""
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r

-- | Returns a unique list of all tags
-- See https://stackoverflow.com/questions/32098328/no-instance-for-database-sqlite-simple-fromfield-fromfield-char
allTags :: IO [[String]]
allTags = do
  hPutStrLn stderr "allTags"
  conn <- open dbFile
  let queryString = Query $ pack "SELECT distinct tag from tags order by tag"
  r <- query_ conn queryString :: IO [[String]]
  close conn
  pure r

allEntries :: IO [Entry]
allEntries = do
  conn <- open dbFile
  r <- query_ conn "SELECT * from entries" :: IO [Entry]
  close conn
  pure r

allCache :: IO [CacheView]
allCache = do
  conn <- open dbFile
  r <- query_ conn "SELECT entry_id, cache_url, cache_content_type, cache_title, cache_body, date, time from cache"
  close conn
  pure r

queryContent :: String -> IO [Entry]
queryContent query = do
  conn <- open dbFile
  let queryString =
        Query $ pack $ "SELECT * FROM entries WHERE content LIKE '%" ++ query ++ "%'"
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r

mkScreenshotFilename = printf "screenshots/%.10d.png"
mkOCRFilename = printf "ocr/%.10d.txt"
ss2ocrFilename x = "ocr/" ++ takeBaseName x ++ ".txt"
ocr2ssFilename x = "screenshots/" ++ takeBaseName x ++ ".png"

crawlerOutput2cache :: [(Entry, String, Maybe WebPage)] -> [CacheEntry]
crawlerOutput2cache out =
  catMaybes $ convert <$> out
  where
    convert (_, _, Nothing) = Nothing
    convert (Entry {..}, url, Just (WebPage title body)) =
      Just
        CacheEntry
          { cacheForeignID = entryID,
            cacheUrl = url,
            cacheContentType = show CachePage, -- TODO - cleanup
            cacheTitle = title,
            cacheBody = body,
            cacheScreenshotFile = mkScreenshotFilename entryID,
            cacheOCRFile = mkOCRFilename entryID
          }

backupDB = do
  now <- getZonedTime
  let timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
  copyFile dbFile (dbFile ++ ".backup." ++ timeStamp ++ ".db")

writeOCR :: [OCREntry] -> IO ()
writeOCR ocrEntries = do
  conn <- open dbFile
  bracketExecute "DROP TABLE IF EXISTS ocr"
  executeNamed
    conn
    ( Query . pack $
        "CREATE TABLE ocr"
          ++ "(ocr_entry_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, "
          ++ "ocr_file TEXT, ocr_content TEXT);"
    )
    []
  mapM_
    ( \OCREntry {..} ->
        executeNamed
          conn
          ( Query . pack $
              "INSERT INTO OCR"
                ++ "       (entry_id, ocr_file, ocr_content) "
                ++ "VALUES (:entryID, :ocrFile, :ocrContent)"
          )
          [ ":entryID" := ocrForeignID,
            ":ocrFile" := ocrFile,
            ":ocrContent" := ocrContent
          ]
    )
    ocrEntries
  close conn

writeCache :: [CacheEntry] -> IO ()
writeCache cacheEntries = do
  now <- getZonedTime
  let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
  let tm = formatTime defaultTimeLocale "%H:%M:%S" now
  let timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
  let tableName = "cache_" ++ timeStamp
  copyFile dbFile (dbFile ++ ".backup." ++ timeStamp ++ ".db")
  conn <- open dbFile

  -- insert metadata entry
  executeNamed
    conn
    ( Query . pack $
        "INSERT INTO cache_meta (table_name, cache_date, cache_time) "
          ++ "VALUES (:tableName, :date, :time)"
    )
    [":tableName" := tableName, ":date" := dt, ":time" := tm]

  -- create new table
  executeNamed
    conn
    ( Query . pack $
        "CREATE TABLE " ++ tableName -- :cacheTable "
          ++ "(cache_entry_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, "
          ++ "cache_url TEXT, "
          ++ "cache_content_type TEXT, cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_ocr_file TEXT);"
    )
    []
  --    [":cacheTable" := tableName]

  -- insert data into new table
  mapM_
    ( \CacheEntry {..} ->
        executeNamed
          conn
          ( Query . pack $
              "INSERT INTO " ++ tableName -- :cacheTable "
                ++ "       (entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_ocr_file) "
                ++ "VALUES (:entryID, :cacheUrl, :cacheContentType, :cacheTitle, :cacheBody, :cacheScreenshotFile, :cacheOCRFile)"
          )
          -- [ ":cacheTable" := tableName,
          [ ":entryID" := cacheForeignID,
            ":cacheUrl" := cacheUrl,
            ":cacheContentType" := cacheContentType,
            ":cacheTitle" := cacheTitle,
            ":cacheBody" := cacheBody,
            ":cacheScreenshotFile" := cacheScreenshotFile,
            ":cacheOCRFile" := cacheOCRFile
          ]
    )
    cacheEntries
  bracketExecute "DROP VIEW IF EXISTS cache"
  bracketExecute $
    "CREATE VIEW cache(cache_entry_id, entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_ocr_file, date, time, content) "
      ++ "as select cache_entry_id, "
      ++ tableName
      ++ ".entry_id as entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_ocr_file, date, time, content "
      ++ "from "
      ++ tableName
      ++ " left join entries on "
      ++ tableName
      ++ ".entry_id=entries.entry_id;"
  close conn

bracketQuery :: FromRow r => String -> IO [r]
bracketQuery queryString = do
  conn <- open dbFile
  r <- query_ conn (Query . pack $ queryString)
  close conn
  pure r

bracketExecute :: String -> IO ()
bracketExecute queryString = do
  conn <- open dbFile
  execute_ conn (Query . pack $ queryString)
  close conn

-- | Wipe all caches and reset the cache metadata table
-- meant to be run interactively from GHCI
wipeCache :: IO ()
wipeCache = do
  backupDB
  r <- bracketQuery "SELECT table_name FROM cache_meta" :: IO [[String]]
  let flattened = concat r
  putStrLn $ "Removing tables \n" ++ show flattened
  mapM_
    ( \x ->
        bracketExecute $ "DROP TABLE IF EXISTS " ++ x
    )
    flattened
  bracketExecute "DROP TABLE IF EXISTS cache_meta"
  bracketExecute "DROP VIEW IF EXISTS cache"
  bracketExecute "CREATE TABLE cache_meta (cache_table_id INTEGER PRIMARY KEY AUTOINCREMENT, table_name TEXT, cache_date TEXT, cache_time TEXT);"

replaceTag :: String -> String -> IO ()
replaceTag fromTag toTag = do
  backupDB
  -- TODO - fill-in
  pure ()

