{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import DB
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setPort,
    setLogger,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Network.Wai.Middleware.Cors (simpleCors)

import Data.Aeson (FromJSON, ToJSON)

-- API Types

data PostNote = PostNote {
  content :: String,
  tags :: [String]
} deriving (Show, Generic)

instance ToJSON PostNote
instance FromJSON PostNote

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

type AllTagsAPI = "all" :> "tags" :> Get '[JSON] [String]
type AllEntriesAPI = "all" :> "entries" :> Get '[JSON] [Entry]
type AllCacheAPI = "all" :> "cache" :> QueryParam "sort" SortBy :> QueryParam "sortdir" SortDir :> Get '[JSON] [CacheView]

type ContentAPI = "content" :> Capture "query" String :> Get '[JSON] [CacheView]

type SubmitAPI = "submit" :> "note" :> ReqBody '[JSON] PostNote :> Post '[JSON] Int64

type FrontendAPI = "frontend" :> Raw

type AllAPI = AllTagsAPI :<|> AllEntriesAPI :<|> AllCacheAPI

type LinkEntryTagsAPI = "link" :> "entry" :> "tags" :> QueryParams "filter" String :> Get '[JSON] [EntryTag]

type CombinedAPI =
  RootAPI :<|> DateAPI :<|> RangeAPI :<|> AllTagsAPI :<|> AllEntriesAPI :<|> AllCacheAPI :<|> ContentAPI :<|> SubmitAPI :<|> FrontendAPI :<|> LinkEntryTagsAPI  

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
    :<|> frontendH
    :<|> linkEntryTagsH

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

allTagsH :: Handler [String]
allTagsH = liftIO allTags 

allEntriesH :: Handler [Entry]
allEntriesH = liftIO allEntries 

allCacheH :: Maybe SortBy -> Maybe SortDir -> Handler [CacheView]
allCacheH sortby sortdir= liftIO (allCache sortby sortdir)

frontendH = serveDirectoryWebApp "./frontend-rs/static/"

queryContentH q = liftIO $ queryContent q :: Handler [CacheView]

linkEntryTagsH filterTag = liftIO $ linkEntryTags filterTag

postNote :: PostNote -> IO Int64
postNote note = do 
  print note
  pure 1

postNoteH note = liftIO $ postNote note

mkApp :: IO Application
mkApp = return $ simpleCors $ serve combinedApi server

runServer :: IO ()
runServer = do
  let port = 3000
  withStdoutLogger $ \aplogger -> do 
    let settings = setPort port $ setLogger aplogger $
                  setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
                  defaultSettings
    runSettings settings =<< mkApp

main :: IO ()
main = runServer
