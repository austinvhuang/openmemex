{-# LANGUAGE DeriveGeneric #-}

module API where

import Control.Monad.IO.Class (liftIO)
import CrawlTools
import DB
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Data.Time (Day (..), TimeOfDay (..), UTCTime (..))
import Date
import GHC.Generics (Generic)
import Servant

data PostSearch = PostSearch {psQuery :: String}
  deriving (Show, Generic)

instance ToJSON PostSearch

instance FromJSON PostSearch

data PostNote = PostNote
  { pnContent :: String,
    pnTags :: [String]
  }
  deriving (Show, Generic)

instance ToJSON PostNote

instance FromJSON PostNote

data PostAnnotation = PostAnnotation
  { contentID :: Int,
    paAnnotation :: String
  }
  deriving (Show, Generic)

instance ToJSON PostAnnotation

instance FromJSON PostAnnotation

-- TODO change to content ID
data PostCompleted = PostCompleted {pcContentID :: Int, pcState :: Bool} deriving (Generic)

instance ToJSON PostCompleted

instance FromJSON PostCompleted

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
newNoteH note = liftIO $ newNote note

-- | Post a link
newLinkH note = liftIO $ newLink note

-- | Write a content annotation
postAnnotationH contentID annotation = liftIO $ postAnnotation contentID annotation

-- | Alter state for content being completed
postCompletedH entryID = liftIO $ postCompleted entryID

-- | Retrieve a list of all topic tags
allTagsH :: Maybe Int -> Handler [String]
allTagsH minCount = liftIO $ allTags minCount

-- | Retrieve all entries
allEventsH :: Handler [Event]
allEventsH = liftIO allEvents

allCacheH ::
  Maybe SortBy ->
  Maybe SortDir ->
  -- | filterTags
  [Text] ->
  -- | limit
  Maybe Int ->
  Maybe Bool ->
  Maybe Day ->
  Maybe Day ->
  Handler [CacheView]
allCacheH
  sortby
  sortdir
  filterTags
  limit
  hideCompleted
  startDate
  endDate =
    liftIO (allCache sortby sortdir filterTags limit hideCompleted startDate endDate)

-- | Retrieve contentID for event ID
event2ContentH entryID = liftIO $ event2Content entryID

-- | Retrieve state for content being completed
getCompletedH entryID = liftIO $ getCompleted entryID

-- | Searchbox retrieval
searchH query = liftIO $ search query

{- Implementations (any DB queries are in DB.hs) -}

-- | Add a note
newNote :: PostNote -> IO Int64
newNote note = do
  putStrLn "Adding note"
  print note
  entryID <- addTextInferDate (pnContent note) (pnTags note)
  pure entryID

-- | Add a note
newLink :: PostNote -> IO Int64
newLink note = do
  putStrLn "Adding link"
  print note
  entryID <- addLinkInferDate (pnContent note) (pnTags note)
  link <- getLink (fromIntegral entryID)
  crawlLinks link
  pure entryID

postAnnotation :: Int -> String -> IO Int64
postAnnotation contentID annotation = do
  putStrLn "Updating annotation"
  addAnnotation $ WriteAnnotation contentID annotation
  pure 0

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
