{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import DB
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Models
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Torch
import Tokenizers

-- API Types

data PostNote = PostNote
  { pnContent :: String,
    pnTags :: [String]
  }
  deriving (Show, Generic)
instance ToJSON PostNote
instance FromJSON PostNote

data PostCompleted = 
  PostCompleted { pcEntryID :: Int } 
  | UndoCompleted { pcEntryID :: Int } deriving (Show, Generic)
instance ToJSON PostCompleted
instance FromJSON PostCompleted

data PostSearch =
  PostSearch { psQuery :: String } deriving (Show, Generic)
instance ToJSON PostSearch
instance FromJSON PostSearch

type RootAPI = Get '[JSON] [String]

type DateAPI =
  "date" :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Get '[JSON] [Entry]

type RangeAPI =
  "range"
    -- start of the range
    :> Capture "year" Int
    :> Capture "month" Int
    :> Capture "day" Int
    -- end of the range
    :> Capture "year" Int
    :> Capture "month" Int
    :> Capture "day" Int
    :> Get '[JSON] [Entry]

type AllTagsAPI = "all" :> "tags" :> QueryParam "min" Int :> Get '[JSON] [String]

type AllEntriesAPI = "all" :> "entries" :> Get '[JSON] [Entry]

type AllCacheAPI =
  "all"
    :> "cache"
    :> QueryParam "sort" SortBy
    :> QueryParam "sortdir" SortDir
    :> QueryParams "tag" Text
    :> QueryParam "limit" Int
    :> Get '[JSON] [CacheView]

type ContentAPI = "content" :> Capture "query" String :> Get '[JSON] [CacheView]

type EntryAPI = "submit" :> "note" :> ReqBody '[JSON] PostNote :> Post '[JSON] Int64
type CompletedAPI = "submit" :> "completed" :> ReqBody '[JSON] PostCompleted :> Post '[JSON] Int64
type SearchAPI = "search" :> Capture "query" String :> Get '[JSON] [CacheView]

type FrontendAPI = "frontend" :> Raw

type LinkEntryTagsAPI =
  "link"
    :> "entry"
    :> "tags"
    :> QueryParams "filter" String
    :> Get '[JSON] [EntryTag]

type HelloTorchAPI =
  "test"
    :> "torch"
    :> Capture "value" Float
    :> Get '[JSON] [TestTorch]

type HelloHuggingfaceAPI =
  "test"
    :> "huggingface"
    :> Capture "value" String
    :> Get '[JSON] [TestHuggingface]

type CombinedAPI =
  RootAPI
    :<|> DateAPI
    :<|> RangeAPI
    :<|> AllTagsAPI
    :<|> AllEntriesAPI
    :<|> AllCacheAPI
    :<|> ContentAPI
    :<|> EntryAPI 
    :<|> CompletedAPI 
    :<|> SearchAPI
    :<|> FrontendAPI
    :<|> LinkEntryTagsAPI
    :<|> HelloTorchAPI
    :<|> HelloHuggingfaceAPI

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

server :: Server CombinedAPI
server =
  getRoot
    :<|> queryDateH
    :<|> queryRangeH
    :<|> allTagsH
    :<|> allEntriesH
    :<|> allCacheH
    :<|> queryContentH
    :<|> postNoteH
    :<|> postCompletedH
    :<|> searchH
    :<|> frontendH
    :<|> linkEntryTagsH
    :<|> helloTorchH
    :<|> helloHuggingfaceH

instance FromHttpApiData SortBy where
  parseUrlPiece value = case value of
    "time" -> Right SortTime
    "url" -> Right SortUrl
    _ -> Left "Invalid sort specification"

instance FromHttpApiData SortDir where
  parseUrlPiece value = case value of
    "fwd" -> Right SortFwd
    "rev" -> Right SortRev
    _ -> Left "Invalid sort direction"

-- Helper functions

-- year month day
date2string = printf "%.4d-%.2d-%.2d" :: String

-- Handlers

getRoot :: Handler [String]
getRoot = return ["n2s API"]

queryDateH y m d = liftIO $ queryDate y m d :: Handler [Entry]

queryRangeH :: Int -> Int -> Int -> Int -> Int -> Int -> Handler [Entry]
queryRangeH y1 m1 d1 y2 m2 d2 = (liftIO $ queryRange y1 m1 d1 y2 m2 d2) :: Handler [Entry]

allTagsH :: Maybe Int -> Handler [String]
allTagsH minCount = liftIO $ allTags minCount

allEntriesH :: Handler [Entry]
allEntriesH = liftIO allEntries

allCacheH :: Maybe SortBy -> Maybe SortDir -> [Text] -> Maybe Int -> Handler [CacheView]
allCacheH sortby sortdir filterTags limit = liftIO (allCache sortby sortdir filterTags limit)

-- frontendH = serveDirectoryFileServer "./frontend-rs/static/."
frontendH = serveDirectoryWebApp "./frontend-rs/static/"
-- what's the difference here?

queryContentH q = liftIO $ queryContent q :: Handler [CacheView]

linkEntryTagsH filterTag = liftIO $ linkEntryTags filterTag

postNote :: PostNote -> IO Int64
postNote note = do
  putStrLn "Adding note"
  print note
  addEntryInferDate (pnContent note) (pnTags note)

postNoteH note = liftIO $ postNote note

postCompleted :: PostCompleted -> IO Int64
postCompleted (PostCompleted entryID) = do
  putStrLn "Marking as complete"
  print entryID
  addCompleted entryID
  pure 0

postCompleted (UndoCompleted entryID) = do
  putStrLn "Removing complete"
  print entryID
  removeCompleted entryID
  pure 0

postCompletedH entryID = liftIO $ postCompleted entryID

searchH query = liftIO $ search query

mkApp :: IO Application
mkApp = pure $ serve combinedApi server

runServer :: IO ()
runServer = do
  let port = 3000
  withStdoutLogger $ \aplogger -> do
    let settings =
          setPort port $
            setLogger aplogger $
              setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
                defaultSettings
    runSettings settings =<< mkApp

main :: IO ()
main = runServer
