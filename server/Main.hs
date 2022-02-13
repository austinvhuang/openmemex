{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- import Network.Wai.Middleware.Cors (simpleCors)

import API
import ArgParser
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import CrawlTools
import DB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day (..), TimeOfDay (..), UTCTime (..))
import Date
import Models
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Options.Applicative (execParser)
import Servant
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import Text.Pretty.Simple
import Torch

-- API Types

type RootAPI = Get '[JSON] [String]

type AllTagsAPI = "all" :> "tags" :> QueryParam "min" Int :> Get '[JSON] [String]

type AllEventsAPI = "all" :> "events" :> Get '[JSON] [Event]

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

type Event2ContentAPI = "event_id" :> Capture "event_id" Int :> Get '[JSON] (Maybe ContentID)

type ContentAPI = "content" :> Capture "query" String :> Get '[JSON] [CacheView]

type WriteNoteAPI = "submit" :> "note" :> ReqBody '[JSON] PostNote :> Post '[JSON] Int64

type WriteLinkAPI = "submit" :> "link" :> ReqBody '[JSON] PostNote :> Post '[JSON] Int64

type WriteAnnotationAPI = "submit" :> "annotation" :> Capture "content_id" Int :> ReqBody '[JSON] String :>  Post '[JSON] Int64

type WriteCompletedAPI = "submit" :> "completed" :> ReqBody '[JSON] PostCompleted :> Post '[JSON] Int64

type GetCompletedAPI = "get" :> "completed" :> Capture "content_id" Int :> Get '[JSON] [Bool]

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

type ConfigAPI =
  "config"
    :> Get '[JSON] [Configuration]

type CombinedAPI =
  RootAPI
    :<|> AllTagsAPI
    :<|> AllEventsAPI
    :<|> AllCacheAPI
    :<|> Event2ContentAPI
    :<|> AllTimestampsAPI
    :<|> WriteNoteAPI
    :<|> WriteLinkAPI
    :<|> WriteAnnotationAPI
    :<|> WriteCompletedAPI
    :<|> GetCompletedAPI
    :<|> SearchAPI
    :<|> FrontendAPI
    :<|> LinkEntryTagsAPI
    :<|> HelloTorchAPI
    :<|> ConfigAPI

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

server :: Configuration -> Server CombinedAPI
server config =
  getRoot
    :<|> allTagsH
    :<|> allEventsH
    :<|> allCacheH
    :<|> event2ContentH
    :<|> allTimestampsH
    :<|> newNoteH
    :<|> newLinkH
    :<|> postAnnotationH
    :<|> postCompletedH
    :<|> getCompletedH
    :<|> searchH
    :<|> frontendH
    :<|> linkEntryTagsH
    :<|> helloTorchH
    :<|> (configH config)

configH :: Configuration -> Handler [Configuration]
configH config = liftIO $ pure [config]

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

mkApp :: Configuration -> IO Application
mkApp config = pure $ serve combinedApi (server config)

runServer :: IO ()
runServer = do
  options <- execParser optionsParser
  pPrint options
  let file = dbFilename options
  check <- doesFileExist file
  when (not check) $ do
    putStrLn $ "Database file " ++ file ++ " not found. Creating a new database file."
    initDB'
  let prt = port options
  let logger = \aplogger -> do
        let settings =
              setPort prt $
                setLogger aplogger $
                  setBeforeMainLoop (hPutStrLn stderr ("OpenMemex server running on port " ++ show prt)) $
                    defaultSettings
        runSettings settings =<< (mkApp options)
  withStdoutLogger logger

main :: IO ()
main = do
  runServer
