{-# LANGUAGE DeriveGeneric #-}

module API where

import Control.Monad.IO.Class (liftIO)
import CrawlTools
import Data.Int (Int64)
import Date
import Data.Time ( Day(..), TimeOfDay(..), UTCTime(..))
import DB
import Servant
import Data.Text (Text, pack, unpack)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data PostSearch =
  PostSearch { psQuery :: String } deriving (Show, Generic)
instance ToJSON PostSearch
instance FromJSON PostSearch

{- Handlers -}

-- | Dummy endpoint for testing
getRoot :: Handler [String]
getRoot = return ["n2s API"]

-- | Get all timestamps used by
allTimestampsH :: Handler [DateTime]
allTimestampsH = liftIO allTimeStamps

-- | Static file serving endpoint
frontendH = serveDirectoryFileServer "./static/."
-- frontendH = serveDirectoryWebApp "./static/"
 
linkEntryTagsH filterTag = liftIO $ linkEntryTags filterTag

-- | Post a note 
postNoteH note = liftIO $ postNote note

-- | Alter state for content being completed
postCompletedH entryID = liftIO $ postCompleted entryID

-- | Retrieve a list of all topic tags
allTagsH :: Maybe Int -> Handler [String]
allTagsH minCount = liftIO $ allTags minCount

-- | Retrieve all entries
allEntriesH :: Handler [Entry]
allEntriesH = liftIO allEntries

allCacheH 
  :: Maybe SortBy 
  -> Maybe SortDir 
  -> [Text] -- ^ filterTags
  -> Maybe Int -- ^ limit
  -> Maybe Bool 
  -> Maybe Day 
  -> Maybe Day 
  -> Handler [CacheView]
allCacheH 
  sortby 
  sortdir 
  filterTags 
  limit 
  hideCompleted 
  startDate
  endDate
  = liftIO (allCache sortby sortdir filterTags limit hideCompleted startDate endDate)
    
-- | Retrieve state for content being completed
getCompletedH entryID = liftIO $ getCompleted entryID

-- | Searchbox retrieval
searchH query = liftIO $ search query

{- Implementations (any DB queries are in DB.hs) -}

-- | Add a note
postNote :: PostNote -> IO Int64
postNote note = do
  putStrLn "Adding note"
  print note
  entryID <- addEntryInferDate (pnContent note) (pnTags note)
  entry <- getEntry (fromIntegral entryID)
  print entry
  crawlEntries entry
  pure entryID

-- | Retrieve content completion (for detail checkbox) flag state
getCompleted :: Int -> IO [Bool]
getCompleted entryID = do
  print entryID
  result <- checkCompleted entryID
  print result
  pure [result]

-- | Set content completion (for detail checkbox) flag state
postCompleted :: PostCompleted -> IO Int64
postCompleted (PostCompleted entryID state) = do
  putStrLn $ "Marking complete as " ++ show state
  print entryID
  case state of
    True -> addCompleted entryID
    False -> removeCompleted entryID
  
  pure 0
