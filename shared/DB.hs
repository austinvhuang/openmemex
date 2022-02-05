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
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Time (Day (..), TimeOfDay (..), UTCTime (..), defaultTimeLocale, diffDays, formatTime, getZonedTime, nominalDiffTimeToSeconds)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock.POSIX
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime (timeOfDayToTime)
import Database.SQLite.Simple
import Date
import Files
import GHC.Generics (Generic)
import GHC.Int (Int64)
import OCR
import SQL
import System.Directory (copyFile, doesFileExist, removeFile)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- Note entries

data Event = Event
  { entryID :: Int,
    date :: String,
    time :: String
  }
  deriving (Eq, Show, Generic)

instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field

instance ToJSON Event

data Link = Link
  { linkEntryID :: Int,
    linkURL :: String
  }
  deriving (Eq, Show, Generic)

instance FromRow Link where
  fromRow = Link <$> field <*> field

instance ToJSON Link

-- Used for DB writes, note entryID is inferred by the database
-- so isn't part of the ADT
data WriteText = WriteText
  { weDate :: String,
    weTime :: String,
    weContent :: String
  }
  deriving (Eq, Show, Generic)

data WriteLink = WriteLink
  { wlDate :: String,
    wlTime :: String,
    wlUrl :: String
  }
  deriving (Eq, Show, Generic)

data WriteAnnotation = WriteAnnotation
  { waDate :: String,
    waTime :: String,
    waContentID :: Int, -- mapping from content table
    waAnnotation :: String
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

-- Simple integer date specifier for uri

data URIDate = URIDate
  { uridYear :: Int,
    uridMonth :: Int,
    uridDay :: Int
  }
  deriving (Show, Generic)

{-
instance FromHttpApiData URIDate where
  parseUrlPiece txt = case day of
    Just d -> Right d
    Nothing -> Left "Error parsing date"
    where
      day = parseTimeM True defaultTimeLocale "%Y-%m-%d" txt :: Maybe Day
-}

-- Cache Tables

data WebPage = WebPage
  { title :: String,
    body :: String
  }
  deriving (Eq, Show, Generic)

data SortBy = SortTime | SortUrl deriving (Show, Generic)

data SortDir = SortFwd | SortRev deriving (Show, Generic)

data URLType = ArxivURL | TwitterURL | PdfURL | GenericURL deriving (Eq, Show)

-- data CacheContentType = CachePage | CacheGenericContent deriving (Show, Generic)

-- API Service View
data CacheView = CacheView
  { cvForeignID :: Int, -- entryID
    cvContentID :: Int,
    cvDate :: String,
    cvTime :: String,
    cvContent :: Maybe String, -- TODO - should this be cvCacheTitle or cvTitle to be consistent with the query?
    cvUrl :: Maybe String,
    cvDisplay :: String,
    cvTitle :: Maybe String,
    cvThumbnailFile :: Maybe String,
    cvScreenshotFile :: Maybe String
  }
  deriving (Show, Generic)

instance FromRow CacheView where
  fromRow = CacheView <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON CacheView

--  database representation
data CacheEntry = CacheEntry
  { cacheForeignID :: Int, -- entryID
    cacheUrl :: String,
    cacheTitle :: String,
    cacheBody :: String,
    cacheScreenshotFile :: String,
    cacheThumbnailFile :: String
  }
  deriving (Show, Generic)

instance FromRow CacheEntry where
  fromRow = CacheEntry <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON CacheEntry

dbFile = "openmemex.db"

-- Helper functions

-- Handlers

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
  r <- bracketQuery' "SELECT DISTINCT date, time from event" :: IO [(String, String)]
  mapM mkDate (mkTime <$> r)

-- | Returns a unique list of all tags
-- See https://stackoverflow.com/questions/32098328/no-instance-for-database-sqlite-simple-fromfield-fromfield-char
allTags :: Maybe Int -> IO [String]
allTags minCount = do
  hPutStrLn stderr "allTags"
  conn <- open dbFile
  let queryString = case minCount of
        Nothing -> Query $ pack "SELECT distinct tag from tag order by tag"
        (Just minCount) -> Query $ pack $ "SELECT tag FROM tag GROUP BY tag HAVING count(*) >= " ++ show minCount ++ " ORDER BY tag"
  r <- query_ conn queryString :: IO [[String]]
  close conn
  pure $ concat r

allEvents :: IO [Event]
allEvents = do
  conn <- open dbFile
  r <- query_ conn "SELECT * from event" :: IO [Event]
  close conn
  pure r

getEvent :: Int -> IO [Event]
getEvent entryID = do
  conn <- open dbFile
  r <- queryNamed conn "SELECT * FROM event WHERE entry_id = :entryID" [":entryID" := entryID] :: IO [Event]
  close conn
  pure r -- list should be of length 1

getLink :: Int -> IO [Link]
getLink entryID = do
  conn <- open dbFile
  r <- queryNamed conn "SELECT * FROM link WHERE entry_id = :entryID" [":entryID" := entryID] :: IO [Link]
  close conn
  pure r -- list should be of length 1 or 0

-- handlers

allCache ::
  Maybe SortBy ->
  Maybe SortDir ->
  [Text] ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Day ->
  Maybe Day ->
  IO [CacheView]
allCache sortby sortdir filterTags limit hideCompleted startDay endDay = do
  -- TODO: support hideCompleted
  conn <- open dbFile
  let tagCond = case filterTags of
        [] -> []
        _ -> [SqlCond ("tag IN ('" ++ (intercalate "','" $ unpack <$> filterTags) ++ "')")]
  let dateStartCond = case startDay of
        Nothing -> []
        Just t -> let (y, m, d) = toGregorian t in [SqlCond $ printf "date >= \"%.4d-%.2d-%.2d\"" y m d]
  let dateEndCond = case endDay of
        Nothing -> []
        Just t -> let (y, m, d) = toGregorian t in [SqlCond $ printf "date <= \"%.4d-%.2d-%.2d\"" y m d]
  let conditions = tagCond ++ dateStartCond ++ dateEndCond
  let query =
        defaultQuery
          { sqlSelect = SqlCol <$> ["cache.entry_id", "cache.content_id", "date", "time", "content", "url", "display", "title", "thumbnail_file", "screenshot_file"],
            sqlFrom =
              if null filterTags
                then SqlFrom "cache"
                else SqlFrom $ "tag LEFT JOIN cache ON cache.entry_id=tag.entry_id",
            sqlLimit = Just limit',
            sqlWhere = conditions,
            sqlOrder = case sortdir of
              Just SortRev -> [SqlOrder "date DESC, time DESC"] -- TODO represent individual termws instead of using a string blob
              Just SortFwd -> [SqlOrder "date, time"]
              Nothing -> [SqlOrder "date DESC, time DESC"] -- TODO represent individual termws instead of using a string blob
          }
  let queryString = sql2string query
  print query
  putStrLn $ "\n" ++ queryString ++ "\n"
  r <- query_ conn (Query . pack $ queryString)
  close conn
  pure r
  where
    limit' = case limit of
      Nothing -> 50
      Just l -> l

linkEntryTags :: [String] -> IO [EntryTag]
linkEntryTags filterTags = do
  conn <- open dbFile
  let query =
        if filterTags == []
          then -- TODO - why does this return a runtime error for the empty case
          -- ConversionFailed {errSQLType = "NULL", errHaskellType = "[Char]", errMessage = "expecting SQLText column type"}
            "SELECT event.entry_id, tag FROM event LEFT JOIN tag on event.entry_id=tag.entry_id"
          else "SELECT event.entry_id, tag FROM event LEFT JOIN tag on event.entry_id=tag.entry_id WHERE tag IN " ++ filterList
  query_ conn (Query . pack $ query)
  where
    filterList = "(" ++ intercalate "," filterTags ++ ")"

crawlerOutput2cache :: [(Link, String, Maybe WebPage)] -> [CacheEntry]
crawlerOutput2cache out =
  catMaybes $ convert <$> out
  where
    convert (_, _, Nothing) = Nothing
    convert (Link {..}, url, Just (WebPage title body)) =
      Just
        CacheEntry
          { cacheForeignID = linkEntryID,
            cacheUrl = url,
            cacheTitle = title,
            cacheBody = body,
            cacheScreenshotFile = mkScreenshotFilename linkEntryID,
            cacheThumbnailFile = mkThumbnailFilename linkEntryID
          }

backupDB = do
  now <- getZonedTime
  let timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
  copyFile dbFile (dbFile ++ ".backup." ++ timeStamp ++ ".db")

data CurrTable = CurrTable
  { currTable :: String
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
  tableName <-
    if length r == 0
      then do
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
                ++ "cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT);"
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

        createCacheView tableName

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
                ++ "       (entry_id, cache_url, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file) "
                ++ "VALUES (:entryID, :cacheUrl, :cacheTitle, :cacheBody, :cacheScreenshotFile, :cacheThumbnailFile)"
          )
          -- [ ":cacheTable" := tableName,
          [ ":entryID" := cacheForeignID,
            ":cacheUrl" := cacheUrl,
            ":cacheTitle" := cacheTitle,
            ":cacheBody" := cacheBody,
            ":cacheScreenshotFile" := cacheScreenshotFile,
            ":cacheThumbnailFile" := cacheThumbnailFile
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
          ++ "cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT);"
    )
    []

  -- insert data into new table
  mapM_
    ( \CacheEntry {..} ->
        executeNamed
          conn
          ( Query . pack $
              "INSERT INTO " ++ tableName -- :cacheTable "
                ++ "       (entry_id, cache_url, cache_title, cache_body, cache_screenshot_file, cache_thumbnail_file) "
                ++ "VALUES (:entryID, :cacheUrl, :cacheTitle, :cacheBody, :cacheScreenshotFile, :cacheThumbnailFile)"
          )
          -- [ ":cacheTable" := tableName,
          [ ":entryID" := cacheForeignID,
            ":cacheUrl" := cacheUrl,
            ":cacheTitle" := cacheTitle,
            ":cacheBody" := cacheBody,
            ":cacheScreenshotFile" := cacheScreenshotFile,
            ":cacheThumbnailFile" := cacheThumbnailFile
          ]
    )
    cacheEntries
  bracketExecute' "DROP INDEX IF EXISTS idx_cache_entry_id;"
  bracketExecute' $ "CREATE UNIQUE INDEX idx_cache_entry_id ON " ++ tableName ++ "(entry_id);"
  bracketExecute' "DROP VIEW IF EXISTS cache"
  createCacheView tableName
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
getDateTime :: IO (String, String)
getDateTime = do
  now <- getZonedTime
  let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
  let tm = formatTime defaultTimeLocale "%H:%M:%S" now
  pure (dt, tm)

addTextInferDate :: String -> [String] -> IO Int64
addTextInferDate entry tags = do
  (dt, tm) <- getDateTime
  entryID <- addText $ WriteText dt tm entry
  mapM_ (addTag entryID) tags
  pure entryID

addLinkInferDate :: String -> [String] -> IO Int64
addLinkInferDate entry tags = do
  (dt, tm) <- getDateTime
  entryID <- addLink $ WriteLink dt tm entry
  mapM_ (addTag entryID) tags
  pure entryID

addText :: WriteText -> IO Int64
addText WriteText {..} = do
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO event (date, time) VALUES (:date, :time)"
    [":date" := weDate, ":time" := weTime]
  r <- lastInsertRowId conn
  executeNamed
    conn
    "INSERT INTO text (entry_id, content) VALUES (:entryID, :content)"
    [":entryID" := r, ":content" := weContent]
  executeNamed
    conn
    (Query . pack $ "INSERT INTO content (entry_id, content_id, is_original) " ++ 
    "VALUES (:entryID, (SELECT IFNULL(MAX(content_id), 0) FROM content), 1)")
    [":entryID" := r]
  executeNamed
    conn
    "INSERT INTO type (entry_id, type) VALUES (:entryID, :type)"
    [":entryID" := r, ":type" := ("TEXT" :: String)]
  executeNamed
    conn
    "INSERT INTO queue (entry_id, status, score) VALUES (:entryID, \"QUEUE\", 0.0)"
    [":entryID" := r]
  close conn
  pure r

addLink :: WriteLink -> IO Int64
addLink WriteLink {..} = do
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO event (date, time) VALUES (:date, :time)"
    [":date" := wlDate, ":time" := wlTime]
  r <- lastInsertRowId conn
  executeNamed
    conn
    "INSERT INTO link (entry_id, url) VALUES (:entryID, :url)"
    [":entryID" := r, ":url" := wlUrl]
  executeNamed
    conn
    (Query . pack $ "INSERT INTO content (entry_id, content_id, is_original) " ++
    "VALUES (:entryID, (SELECT IFNULL(MAX(content_id), 0) FROM content), 1)")
    [":entryID" := r]
  executeNamed
    conn
    "INSERT INTO type (entry_id, type) VALUES (:entryID, :type)"
    [":entryID" := r, ":type" := ("LINK" :: String)]
  executeNamed
    conn
    "INSERT INTO queue (entry_id, status, score) VALUES (:entryID, \"QUEUE\", 0.0)"
    [":entryID" := r]
  close conn
  pure r

addAnnotation :: WriteAnnotation -> IO Int64
addAnnotation WriteAnnotation {..} = do
  (dt, tm) <- getDateTime
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO event (date, time) VALUES (:date, :time)"
    [":date" := dt, ":time" := tm]
  r <- lastInsertRowId conn
  executeNamed
    conn
    "INSERT INTO annotation (entry_id, annotation) VALUES (:entryID, :annotation)"
    [":entryID" := r, ":url" := waAnnotation]
  executeNamed
    conn
    "INSERT INTO content (entry_id, content_id, is_original) VALUES (:entryID, :contentID, 0)"
    [":entryID" := r, ":contentID" := waContentID]
  executeNamed
    conn
    "INSERT INTO type (entry_id, type) VALUES (:entryID, :type)"
    [":entryID" := r, ":type" := ("ANNOTATION_UPDATE" :: String)]
  close conn
  pure undefined

addTag :: Int64 -> String -> IO Int64
addTag entryID tag = do
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO tag (entry_id, tag) VALUES (:entryID, :tag)"
    [":entryID" := entryID, ":tag" := tag]
  r <- lastInsertRowId conn
  close conn
  pure r

addCompleted :: Int -> IO Int64
addCompleted contentID = do
  (dt, tm) <- getDateTime
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO event (date, time) VALUES (:date, :time)"
    [":date" := dt, ":time" := tm]
  r <- lastInsertRowId conn
  executeNamed
    conn
    "INSERT INTO content (entry_id, content_id, is_original) VALUES (:entryID, :contentID, 0)"
    [":entryID" := r, ":contentID" := contentID]
  executeNamed
    conn
    "INSERT INTO type (entry_id, type) VALUES (:entryID, \"QUEUE_UPDATE\")"
    [":entryID" := r]
  executeNamed
    conn
    "INSERT queue (entry_id, status, score) VALUES (:entryID, \"DONE\", 0.0)"
    [":entryID" := r]
  close conn
  pure r

removeCompleted :: Int -> IO Int64
removeCompleted contentID = do
  (dt, tm) <- getDateTime
  conn <- open dbFile
  executeNamed
    conn
    "INSERT INTO event (date, time) VALUES (:date, :time)"
    [":date" := dt, ":time" := tm]
  r <- lastInsertRowId conn
  executeNamed
    conn
    "INSERT queue(entry_id, status, score) VALUES (:entryID, \"QUEUE\", 0.0)"
    [":entryID" := r]
    {-
  executeNamed
    conn
    "UPDATE queue SET status = \"QUEUE\" WHERE entry_id = :entryID"
    [":entryID" := entryID]
    -}
  close conn
  pure 0

checkCompleted :: Int -> IO Bool
checkCompleted entryID = do
  conn <- open dbFile
  r <- query_ conn (Query . pack $ "SELECT status FROM queue WHERE entry_id == " ++ show entryID) :: IO [[String]]
  close conn
  print r
  -- TODO - make this safe
  pure $ (r !! 0) !! 0 == "DONE"

search :: String -> IO [CacheView]
search query = do
  putStrLn $ "Searching for '" ++ query ++ "'"
  conn <- open dbFile
  let queryString = if (query == "") then emptyQuery else stdQuery
  print queryString
  query_ conn queryString :: IO [CacheView]
  where
    stdQuery =
      Query $
        pack
          ( "SELECT DISTINCT cache.entry_id, date, time, content, url, display, title, thumbnail_file, screenshot_file "
              ++ "FROM cache "
              ++ "LEFT JOIN tag ON cache.entry_id=tag.entry_id "
              ++ "WHERE url LIKE '%"
              ++ query
              ++ "%' OR title LIKE '%"
              ++ query
              ++ "%' OR tag.tag LIKE '%"
              ++ query
              ++ "%' "
              ++ "ORDER BY coalesce(datetime(\"date\"), datetime(\"time\")) DESC"
          )
    emptyQuery =
      Query $
        pack
          ( "SELECT DISTINCT cache.entry_id, date, time, content, url, display, title, thumbnail_file, screenshot_file "
              ++ "FROM cache "
              ++ "ORDER BY coalesce(datetime(\"date\"), datetime(\"time\")) DESC"
          )

wipeTesting :: IO ()
wipeTesting = do
  putStrLn "removing events and tag where tag==\"testing\""
  bracketExecute' "delete from event where event.entry_id in (select tag.entry_id from tag where tag==\"testing\")"
  bracketExecute' "delete from tag where tag==\"testing\""

initDB' = runReaderT initDB (Sqlite dbFile)

createCacheView :: String -> IO ()
createCacheView tableName = do
  let createView =
        "CREATE VIEW IF NOT EXISTS cache(entry_id, content_id, date, time, " -- event
          ++ "content, url, display, " -- text, link, coalesced(text+link)
          ++ "title, screenshot_file, thumbnail_file) " -- cache_*
          ++ "AS SELECT event.entry_id, content.content_id, date, time, content, url, "
          ++ "COALESCE((SELECT content FROM text WHERE url IS NULL AND text.entry_id=event.entry_id), "
          ++ "         (SELECT cache_title FROM "
          ++ tableName
          ++ " WHERE content IS NULL AND "
          ++ tableName
          ++ ".entry_id=event.entry_id), \"\") AS display, "
          ++ "cache_title, cache_screenshot_file, cache_thumbnail_file "
          ++ " FROM content LEFT JOIN event ON event.entry_id=content.entry_id "
          ++ "LEFT JOIN "
          ++ tableName
          ++ " ON "
          ++ tableName
          ++ ".entry_id=event.entry_id"
          ++ " LEFT JOIN text on event.entry_id=text.entry_id"
          ++ " LEFT JOIN link on event.entry_id=link.entry_id"
  putStrLn $ "Creating cache view:\n" ++ show createView
  bracketExecute' createView

initDB :: ReaderT Sqlite IO ()
initDB = do
  dbFile <- asks sqliteFile
  liftIO $ do
    fileExists <- doesFileExist dbFile
    when fileExists $ do
      copyFile dbFile (dbFile ++ ".backup")
      removeFile dbFile
    when (not fileExists) $ do
      writeFile dbFile ""
    conn <- open dbFile

    -- Schema version
    bracketExecute' "PRAGMA user_version = 2;"

    -- CREATE TABLES
    dropTables'
      [ "event",
        "type",
        "content",
        "tag",
        "annotation",
        "queue",
        "text",
        "link",
        "artifact",
        "cache_meta"
      ]

    -- Tables: universal data - event, type, tags, content
    bracketExecute' "CREATE TABLE event (entry_id INTEGER PRIMARY KEY AUTOINCREMENT, date TEXT, time TEXT);"
    bracketExecute' "CREATE TABLE type (entry_id INTEGER, type TEXT CHECK (type IN ('TEXT', 'TEXT_UPDATE', 'IMAGE', 'AUDIO', 'LINK', 'ANNOTATION_UPDATE',  'QUEUE_UPDATE', 'OTHER')), UNIQUE(entry_id, type) );"

    bracketExecute' "CREATE TABLE content (entry_id INTEGER PRIMARY KEY UNIQUE, content_id INTEGER, is_original INTEGER);"

    -- Tables: annotation TODO - should these be linked to content_id?
    bracketExecute' "CREATE TABLE tag (tag_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, tag TEXT);"
    bracketExecute' "CREATE TABLE annotation(entry_id INTEGER UNIQUE,  annotation TEXT);"
    bracketExecute' "CREATE TABLE queue(entry_id INTEGER, status TEXT CHECK (status IN ('QUEUE', 'IN_PROGRESS', 'DONE')), score REAL);"

    -- TODO group by content_ids, get most recent (ie current) queue value 
    bracketExecute' "DROP VIEW IF EXISTS queue_status;"
    {-
    bracketExecute' ("CREATE VIEW IF NOT EXISTS queue_state(content_id, status, score) " ++
                     "AS SELECT content.content_id, content.status, content.score FROM " ++
                     "content LEFT JOIN queue ON content.entry_id=queue.entry_id " ++ 
                     "GROUP BY content.content_id"
    -}

    -- Tables: type-specific data - text, link, artifact
    bracketExecute' "CREATE TABLE text (entry_id INTEGER UNIQUE, content TEXT);"
    bracketExecute' "CREATE TABLE link (entry_id INTEGER UNIQUE, url TEXT);"
    bracketExecute' "CREATE TABLE artifact (entry_id INTEGER UNIQUE, artifact BLOB);"

    -- Tables: caching
    bracketExecute' "CREATE TABLE cache_meta (cache_table_id INTEGER PRIMARY KEY AUTOINCREMENT, table_name TEXT, cache_date TEXT, cache_time TEXT);"

    -- CREATE INDICES

    createIndices'
      [ Index "idx_event_entry_id" "event" "entry_id" True,
        Index "idx_event_time" "event" "time" False,
        Index "idx_event_date" "event" "date" False,
        Index "idx_type_event_id" "type" "entry_id" False,
        Index "idx_type_tag" "type" "type" False,
        Index "idx_tag_event_id" "tag" "entry_id" False,
        Index "idx_tag_tag" "tag" "tag" False,
        Index "idx_content_event_id" "content" "entry_id" False,
        Index "idx_content_content_id" "content" "content_id" False,
        Index "idx_annotation_entry_id" "annotation" "entry_id" True,
        Index "idx_annotation_annotation" "annotation" "annotation" False,
        Index "idx_queue_entry_id" "queue" "entry_id" False,
        Index "idx_text_entry_id" "text" "entry_id" True,
        Index "idx_text_content" "text" "content" False,
        Index "idx_link_entry_id" "link" "entry_id" False,
        Index "idx_link_url" "link" "url" False,
        Index "idx_artifact_entry_id" "artifact" "entry_id" False
      ]

    now <- getZonedTime
    let dt = formatTime defaultTimeLocale "%Y-%m-%d" now
        tm = formatTime defaultTimeLocale "%H:%M:%S" now
        timeStamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
        tableName = "cache_" ++ timeStamp
    bracketExecute' $
      "CREATE TABLE " ++ tableName
        ++ "(cache_entry_id INTEGER PRIMARY KEY AUTOINCREMENT, entry_id INTEGER, "
        ++ "cache_url TEXT, "
        ++ "cache_title TEXT, cache_body TEXT, cache_screenshot_file TEXT, cache_thumbnail_file TEXT);"
    bracketExecute' $ "DROP TABLE IF EXISTS " ++ tableName ++ ";"
    bracketExecute' "DROP VIEW IF EXISTS cache;"

    createCacheView tableName

    addTextInferDate "Memex Created." [] -- TODO get rid of this hack
    appendCache []
    pure ()

bracketExecute' :: String -> IO ()
bracketExecute' q = runReaderT (bracketExecute q) (Sqlite dbFile)

bracketQuery' :: FromRow r => String -> IO [r]
bracketQuery' q = runReaderT (bracketQuery q) (Sqlite dbFile)

dropTables' :: [String] -> IO ()
dropTables' tables = runReaderT (dropTables tables) (Sqlite dbFile)

createIndices' :: [Index] -> IO ()
createIndices' indexList = runReaderT (createIndices indexList) (Sqlite dbFile)
