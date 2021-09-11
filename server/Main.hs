{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Data.Int (Int64)
import Data.Time ( Day(..), TimeOfDay(..), UTCTime(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
-- import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import System.IO (hPutStrLn, stderr)

import DB
import Torch
import Tokenizers
import CrawlTools
import Date
import API
import Models

-- API Types

type RootAPI = Get '[JSON] [String]

type AllTagsAPI = "all" :> "tags" :> QueryParam "min" Int :> Get '[JSON] [String]

type AllEntriesAPI = "all" :> "entries" :> Get '[JSON] [Entry]

type AllTimestampsAPI = "all" :> "timestamps" :> Get '[JSON] [DateTime]

type AllCacheAPI =
  "all"
    :> "cache"
    :> QueryParam "sort" SortBy
    :> QueryParam "sortdir" SortDir
    :> QueryParams "tag" Text
    :> QueryParam "limit" Int
    :> QueryParam "hidecompleted" Bool
    :> QueryParam "startDate" Day
    :> QueryParam "endDate" Day
    :> Get '[JSON] [CacheView]

type ContentAPI = "content" :> Capture "query" String :> Get '[JSON] [CacheView]

type EntryAPI = "submit" :> "note" :> ReqBody '[JSON] PostNote :> Post '[JSON] Int64

type CompletedAPI = "submit" :> "completed" :> ReqBody '[JSON] PostCompleted :> Post '[JSON] Int64
  
type GetCompletedAPI = "get" :> "completed" :> Capture "entry_id" Int :> Get '[JSON] [Bool]
  
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
    :<|> AllTagsAPI
    :<|> AllEntriesAPI
    :<|> AllCacheAPI
    :<|> AllTimestampsAPI
    :<|> ContentAPI
    :<|> EntryAPI 
    :<|> CompletedAPI 
    :<|> GetCompletedAPI 
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
    :<|> allTagsH
    :<|> allEntriesH
    :<|> allCacheH
    :<|> allTimestampsH
    :<|> queryContentH
    :<|> postNoteH
    :<|> postCompletedH
    :<|> getCompletedH
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
