{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB where

import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime, Day(..), TimeOfDay(..), UTCTime(..), diffDays, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX
import Data.Time.Format (parseTimeM)
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (timeOfDayToTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import GHC.Int (Int64)
import OCR
import SQL
import System.Directory (copyFile, removeFile)
import System.IO (hPutStrLn, stderr)
import Date

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

-- Used for DB writes, note entryID is inferred by the database
-- so isn't part of the ADT
data WriteEntry = WriteEntry
  { weDate :: String,
    weTime :: String,
    weContent :: String
  }
  deriving (Eq, Show, Generic)

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

-- Link entries to tags

data EntryTag = EntryTag
  { etEntryID :: Int,
    etTag :: String
  }
  deriving (Show, Generic)

instance FromRow EntryTag where
  fromRow = EntryTag <$> field <*> field

instance ToJSON EntryTag

-- Cache Tables

data WebPage = WebPage
  { title :: String,
    body :: String
  }
  deriving (Eq, Show, Generic)

data SortBy = SortTime | SortUrl deriving (Show, Generic)

data SortDir = SortFwd | SortRev deriving (Show, Generic)

data URLType = ArxivURL | TwitterURL | PdfURL | GenericURL deriving (Eq, Show)

data CacheContentType = CachePage | CacheGenericContent deriving (Show, Generic)

-- API Service View
data CacheView = CacheView
  { cvForeignID :: Int, -- entryID
    cvUrl :: Maybe String,
    cvContentType :: Maybe String, -- CacheContentType,
    cvContent :: Maybe String, -- TODO - should this be cvCacheTitle or cvTitle to be consistent with the query?
    cvDate :: String,
    cvTime :: String,
    cvScreenshotFile :: Maybe String,
    cvThumbnailFile :: Maybe String
  }
  deriving (Show, Generic)

instance FromRow CacheView where
  fromRow = CacheView <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON CacheView

--  database representation
data CacheEntry = CacheEntry
  { cacheForeignID :: Int, -- entryID
    cacheUrl :: String,
    cacheContentType :: String, -- CacheContentType,
    cacheTitle :: String,
    cacheBody :: String,
    cacheScreenshotFile :: String,
    cacheThumbnailFile :: String,
    cacheOCRFile :: String
  }
  deriving (Show, Generic)

instance FromRow CacheEntry where
  fromRow = CacheEntry <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON CacheEntry

data OCREntry = OCREntry
  { ocrForeignID :: Int, -- entryID
    ocrFile :: String,
    ocrContent :: String
  }
  deriving (Generic)

instance FromRow OCREntry where
  fromRow = OCREntry <$> field <*> field <*> field

data PostNote = PostNote
  { pnContent :: String,
    pnTags :: [String]
  }
  deriving (Show, Generic)
instance ToJSON PostNote
instance FromJSON PostNote

data PostCompleted = PostCompleted { pcEntryID :: Int, pcState :: Bool} deriving Generic
instance ToJSON PostCompleted
instance FromJSON PostCompleted

dbFile = "note2self.db"

-- Helper functions

-- Handlers

getCompleted :: Int -> IO [Bool]
getCompleted entryID = do
  print entryID
  result <- checkCompleted entryID
  print result
  pure [result]

postCompleted :: PostCompleted -> IO Int64
postCompleted (PostCompleted entryID state) = do
  putStrLn $ "Marking complete as " ++ show state
  print entryID
  case state of
    True -> addCompleted entryID
    False -> removeCompleted entryID
  
  pure 0

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

mkTime :: (String, String) -> (Day, TimeOfDay)
mkTime (d, tod) = (day, timeOfDay)
  where
    -- warning - no exception checking
    Just day = parseTimeM True defaultTimeLocale "%Y-%m-%d" d :: Maybe Day
    Just timeOfDay = parseTimeM True defaultTimeLocale "%H:%M:%S" tod :: Maybe TimeOfDay

mkDate :: (Day, TimeOfDay) -> IO DateTime
mkDate (day, timeOfDay) = do
  let (year, month, dayOfMonth) = toGregorian day
      year' = fromIntegral year
      (hour, min, sec) = (todHour timeOfDay, todMin timeOfDay, round . todSec $ timeOfDay)
      utc = UTCTime day (timeOfDayToTime $ timeOfDay)
      utc' = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ utc
  pure $ DateTime (year', month, dayOfMonth) (hour, min, sec) utc'

-- | Get time stamps of all entries
allTimeStamps :: IO [DateTime]
allTimeStamps = do
  r <- bracketQuery' "SELECT DISTINCT date, time from entries" :: IO [(String, String)]
  mapM mkDate (mkTime <$> r)


-- | Returns a unique list of all tags
-- See https://stackoverflow.com/questions/32098328/no-instance-for-database-sqlite-simple-fromfield-fromfield-char
allTags :: Maybe Int -> IO [String]
allTags minCount = do
  hPutStrLn stderr "allTags"
  conn <- open dbFile
  let queryString = case minCount of
        Nothing -> Query $ pack "SELECT distinct tag from tags order by tag"
        (Just minCount) -> Query $ pack $ "SELECT tag FROM tags GROUP BY tag HAVING count(*) > " ++ show minCount ++ " ORDER BY tag"
  r <- query_ conn queryString :: IO [[String]]
  close conn
  pure $ concat r

allEntries :: IO [Entry]
allEntries = do
  conn <- open dbFile
  r <- query_ conn "SELECT * from entries" :: IO [Entry]
  close conn
  pure r

getEntry :: Int -> IO [Entry]
getEntry entryID = do
  conn <- open dbFile
  r <- queryNamed conn "SELECT * FROM entries WHERE entry_id = :entryID" [":entryID" := entryID] :: IO [Entry]
  close conn
  pure r -- list should be of length 1

-- handlers

allCache :: Maybe SortBy -> Maybe SortDir -> [Text] -> Maybe Int -> Maybe Bool -> IO [CacheView]
allCache sortby sortdir filterTags limit hideCompleted = do
  -- TODO: support hideCompleted
  conn <- open dbFile
  let query =
        defaultQuery
          { sqlSelect = SqlCol <$> ["cache.entry_id", "cache_url", "cache_content_type", "cache_title", "date", "time", "cache_screenshot_file", "cache_thumbnail_file"],
            sqlFrom = SqlFrom $ "tags LEFT JOIN cache ON cache.entry_id=tags.entry_id",
            sqlLimit = Just 50,
            sqlWhere = if (null filterTags) 
                        then [] 
                        else [SqlCond (if null filterTags then "" else "tag IN ('" ++ (intercalate "','" $ unpack <$> filterTags) ++ "')")],
            sqlOrder = [SqlOrder "date DESC, time DESC"] -- TODO represent individual termws instead of using a string blob
          }
  let queryString = sql2string query
  print query
  putStrLn $ "\n" ++ queryString ++ "\n"
  r <- query_ conn (Query . pack $ queryString)
  close conn
  pure r

queryContent :: String -> IO [CacheView]
queryContent query = do
  conn <- open dbFile
  let queryString =
        Query $ pack $ "SELECT entry_id, cache_url, cache_content_type, cache_title, date,  time, cache_screenshot_file, cache_thumbnail_file from cache ORDER BY coalesce(datetime(\"date\"), datetime(\"time\")) DESC WHERE cache_title LIKE '%" ++ query ++ "%'"
  r <- query_ conn queryString
  close conn
  pure r

linkEntryTags :: [String] -> IO [EntryTag]
linkEntryTags filterTags = do
  conn <- open dbFile
  let query =
        if filterTags == []
          then -- TODO - why does this return a runtime error for the empty case
          -- ConversionFailed {errSQLType = "NULL", errHaskellType = "[Char]", errMessage = "expecting SQLText column type"}
            "SELECT entries.entry_id, tag FROM entries LEFT JOIN tags on entries.entry_id=tags.entry_id"
          else "SELECT entries.entry_id, tag FROM entries LEFT JOIN tags on entries.entry_id=tags.entry_id WHERE tag IN " ++ filterList
  query_ conn (Query . pack $ query)
  where
    filterList = "(" ++ intercalate "," filterTags ++ ")"

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
            cacheThumbnailFile = mkThumbnailFilename entryID,
            cacheOCRFile = mkOCRFilename entryID
          }

backupDB = do
  now <- getZonedTime
  let timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
  copyFile dbFile (dbFile ++ ".backup." ++ timeStamp ++ ".db")

writeOCR :: [OCREntry] -> IO ()
writeOCR ocrEntries = do
  conn <- open dbFile
  bracketExecute' "DROP TABLE IF EXISTS ocr"
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

data CurrTable = CurrTable
  { 
    currTable :: String
  }
  deriving (Eq, Show, Generic)

instance FromRow CurrTable where
  fromRow = CurrTable <$> field

appendCache :: [CacheEntry] -> IO ()
appendCache cacheEntries = do
  now <- getZonedTime
  conn <- open dbFile
  -- get the most recent cache snapshot
  r <- query_ conn $ Query "SELECT table_name FROM cache_meta ORDER BY cache_date, cache_time DESC LIMIT 1;"
  tableName <- if length r == 0 then do
      putStrLn "creating new cache table"
      let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
      let tm = formatTime defaultTimeLocale "%H:%M:%S" now
      let timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      let tableName = "cache_" ++ timeStamp
      executeNamed
        conn
        ( Query . pack $
            "CREATE TABLE " ++ tableName -- :cacheTable "
              ++ "(cache_entry_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, "
              ++ "cache_url TEXT, "
              ++ "cache_content_type TEXT, cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT, cache_ocr_file TEXT);"
        )
        []

      executeNamed
        conn
        ( Query . pack $
            "INSERT INTO cache_meta (table_name, cache_date, cache_time) "
              ++ "VALUES (:tableName, :date, :time)"
        )
        [":tableName" := tableName, ":date" := dt, ":time" := tm]
      bracketExecute' "DROP VIEW IF EXISTS cache"
      bracketExecute' $
        "CREATE VIEW IF NOT EXISTS cache(cache_entry_id, entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content) "
          ++ "as select cache_entry_id, "
          ++ "entries.entry_id as entry_id, cache_url, cache_content_type, coalesce(cache_title, entries.content), cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content "
          ++ "from entries"
          ++ " left join "
          ++ tableName
          ++ " on "
          ++ tableName
          ++ ".entry_id=entries.entry_id;"
      pure tableName

    else do
      let (CurrTable tableName) = (r !! 0)
      pure tableName

  putStrLn $ "appending to " ++ tableName
  mapM_
    ( \CacheEntry {..} ->
        executeNamed
          conn
          ( Query . pack $
              "INSERT INTO " ++ tableName -- :cacheTable "
                ++ "       (entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file) "
                ++ "VALUES (:entryID, :cacheUrl, :cacheContentType, :cacheTitle, :cacheBody, :cacheScreenshotFile, :cacheThumbnailFile, :cacheOCRFile)"
          )
          -- [ ":cacheTable" := tableName,
          [ ":entryID" := cacheForeignID,
            ":cacheUrl" := cacheUrl,
            ":cacheContentType" := cacheContentType,
            ":cacheTitle" := cacheTitle,
            ":cacheBody" := cacheBody,
            ":cacheScreenshotFile" := cacheScreenshotFile,
            ":cacheThumbnailFile" := cacheThumbnailFile,
            ":cacheOCRFile" := cacheOCRFile
          ]
    )
    cacheEntries
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
          ++ "cache_content_type TEXT, cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT, cache_ocr_file TEXT);"
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
                ++ "       (entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file) "
                ++ "VALUES (:entryID, :cacheUrl, :cacheContentType, :cacheTitle, :cacheBody, :cacheScreenshotFile, :cacheThumbnailFile, :cacheOCRFile)"
          )
          -- [ ":cacheTable" := tableName,
          [ ":entryID" := cacheForeignID,
            ":cacheUrl" := cacheUrl,
            ":cacheContentType" := cacheContentType,
            ":cacheTitle" := cacheTitle,
            ":cacheBody" := cacheBody,
            ":cacheScreenshotFile" := cacheScreenshotFile,
            ":cacheThumbnailFile" := cacheThumbnailFile,
            ":cacheOCRFile" := cacheOCRFile
          ]
    )
    cacheEntries
  bracketExecute' "DROP INDEX IF EXISTS idx_cache_entry_id;"
  bracketExecute' $ "CREATE UNIQUE INDEX idx_cache_entry_id ON " ++ tableName ++ "(entry_id);"
  bracketExecute' "DROP VIEW IF EXISTS cache"
  bracketExecute' $
    "CREATE VIEW cache(cache_entry_id, entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content) "
      ++ "as select cache_entry_id, "
      ++ "entries.entry_id as entry_id, cache_url, cache_content_type, coalesce(cache_title, entries.content), cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content "
      ++ "from entries"
      ++ " left join "
      ++ tableName
      ++ " on "
      ++ tableName
      ++ ".entry_id=entries.entry_id;"
  close conn

-- | Wipe all caches and reset the cache metadata table
-- meant to be run interactively from GHCI
wipeCache :: IO ()
wipeCache = do
  backupDB
  r <- bracketQuery' "SELECT table_name FROM cache_meta" :: IO [[String]]
  let flattened = concat r
  putStrLn $ "Removing tables \n" ++ show flattened
  mapM_
    ( \x ->
        bracketExecute' $ "DROP TABLE IF EXISTS " ++ x
    )
    flattened
  bracketExecute' "DROP TABLE IF EXISTS cache_meta"
  bracketExecute' "DROP VIEW IF EXISTS cache"
  bracketExecute' "CREATE TABLE cache_meta (cache_table_id INTEGER PRIMARY KEY AUTOINCREMENT, table_name TEXT, cache_date TEXT, cache_time TEXT);"

replaceTag :: String -> String -> IO ()
replaceTag fromTag toTag = do
  backupDB
  -- TODO - fill-in
  pure ()

-- | Get current date and time
getDateTime = do
  now <- getZonedTime
  let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
  let tm = formatTime defaultTimeLocale "%H:%M:%S" now
  pure (dt, tm)

addEntryInferDate :: String -> [String] -> IO Int64
addEntryInferDate entry tags = do
  (dt, tm) <- getDateTime
  entryID <- addEntry $ WriteEntry dt tm entry
  mapM_ (addTag entryID) tags
  pure entryID

addEntry :: WriteEntry -> IO Int64
addEntry WriteEntry {..} = do
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO entries (date, time, content) VALUES (:date, :time, :content)"
    [":date" := weDate, ":time" := weTime, ":content" := weContent]
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

addCompleted :: Int -> IO Int64
addCompleted entryID = do
  (dt, tm) <- getDateTime
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO completed (entry_id, completed_date, completed_time) VALUES (:entryID, :date, :time)"
    [":entryID" := entryID, ":date" := dt, ":time" := tm]
  r <- lastInsertRowId conn
  close conn
  pure r

removeCompleted :: Int -> IO Int64
removeCompleted entryID = do
  conn <- open dbFile
  executeNamed
    conn
    "DELETE FROM completed WHERE entry_id = :entryID"
    [":entryID" := entryID]
  close conn
  pure 0

checkCompleted :: Int -> IO Bool
checkCompleted entryID = do
  conn <- open dbFile
  r <- query_ conn (Query . pack $ "SELECT completed_date FROM completed WHERE entry_id == " ++ show entryID) :: IO [[String]]
  close conn
  pure $ if null r then False else (not $ null (r !! 0))

search :: String -> IO [CacheView]
search query = do
  putStrLn $ "Searching for " ++ query
  conn <- open dbFile
  let queryString = Query $ pack (
                    "SELECT DISTINCT cache.entry_id, cache_url, cache_content_type, cache_title, date, time, cache_screenshot_file, cache_thumbnail_file " ++
                    "FROM cache " ++
                    "LEFT JOIN tags ON cache.entry_id=tags.entry_id " ++ 
                    "WHERE cache_url LIKE '%" ++ query ++ "%' OR cache_title LIKE '%" ++ query ++ "%' OR tags.tag LIKE '%" ++ query ++ "%' " ++
                    "ORDER BY coalesce(datetime(\"date\"), datetime(\"time\")) DESC")
  print queryString
  query_ conn queryString :: IO [CacheView]

wipeTesting :: IO ()
wipeTesting = do
  putStrLn "removing entries and tags where tags==\"testing\""
  bracketExecute' "delete from entries where entries.entry_id in (select tags.entry_id from tags where tag==\"testing\")"
  bracketExecute' "delete from tags where tag==\"testing\""

initDB' = runReaderT initDB (Sqlite dbFile)

initDB :: ReaderT Sqlite IO ()
initDB = do
  dbFile <- asks sqliteFile
  liftIO $ do
      copyFile dbFile (dbFile ++ ".backup")
      removeFile dbFile
      conn <- open dbFile
      dropTables' ["entries", "tags", "cache_meta", "annotations"]
      bracketExecute' "CREATE TABLE entries (entry_id INTEGER PRIMARY KEY AUTOINCREMENT, date TEXT, time TEXT, content TEXT);"
      bracketExecute' "CREATE TABLE tags (tag_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, tag TEXT);"
      bracketExecute' "CREATE TABLE cache_meta (cache_table_id INTEGER PRIMARY KEY AUTOINCREMENT, table_name TEXT, cache_date TEXT, cache_time TEXT);"
      bracketExecute' "CREATE TABLE annotations(annotation_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, annotation_date TEXT, annotation_time TEXT, annotation_content TEXT);"
      createIndices' [Index "idx_tags_entry_id" "tags" "entry_id" False,
                     Index "idx_entries_time" "entries" "time" False,
                     Index "idx_entries_date" "entries" "date" False,
                     Index "idx_tags_tag" "tags" "tag" False,
                     Index "idx_entries_entry_id" "entries" "entry_id" True,
                     Index "idx_annotations_date" "annotations" "annotation_date" False,
                     Index "idx_annotations_time" "annotations" "annotation_time" False,
                     Index "idx_annotations_entry_id" "annotations" "entry_id" False
                     ]
      now <- getZonedTime
      let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
          tm = formatTime defaultTimeLocale "%H:%M:%S" now
          timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
          tableName = "cache_" ++ timeStamp
      bracketExecute' $ "CREATE TABLE " ++ tableName
                  ++ "(cache_entry_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, "
                  ++ "cache_url TEXT, "
                  ++ "cache_content_type TEXT, cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT, cache_ocr_file TEXT);"
      bracketExecute' $ "DROP TABLE IF EXISTS " ++ tableName ++ ";"
      bracketExecute' "DROP VIEW IF EXISTS cache;"
      bracketExecute' $
            "CREATE VIEW IF NOT EXISTS cache(cache_entry_id, entry_id, cache_url, cache_content_type, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content) "
              ++ "as select cache_entry_id, "
              ++ "entries.entry_id as entry_id, cache_url, cache_content_type, coalesce(cache_title, entries.content), cache_body, cache_screenshot_file, cache_thumbnail_file, cache_ocr_file, date, time, content "
              ++ "from entries"
              ++ " left join "
              ++ tableName
              ++ " on "
              ++ tableName
              ++ ".entry_id=entries.entry_id;"

bracketExecute' :: String -> IO ()
bracketExecute' q = runReaderT (bracketExecute q) (Sqlite dbFile)

bracketQuery' :: FromRow r => String -> IO [r]
bracketQuery' q = runReaderT (bracketQuery q)  (Sqlite dbFile)

dropTables' :: [String] -> IO ()
dropTables' tables = runReaderT (dropTables tables) (Sqlite dbFile)

createIndices' :: [Index] -> IO ()
createIndices' indexList = runReaderT (createIndices indexList) (Sqlite dbFile)
