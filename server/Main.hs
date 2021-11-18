{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
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
import Options.Applicative (execParser)
import Servant
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import Text.Pretty.Simple

import ArgParser
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

type ConfigAPI =
  "config"
    :> Get '[JSON] [Configuration]

type CombinedAPI =
  RootAPI
    :<|> AllTagsAPI
    :<|> AllEntriesAPI
    :<|> AllCacheAPI
    :<|> AllTimestampsAPI
    :<|> EntryAPI 
    :<|> CompletedAPI 
    :<|> GetCompletedAPI 
    :<|> SearchAPI
    :<|> FrontendAPI
    :<|> LinkEntryTagsAPI
    :<|> HelloTorchAPI
    :<|> HelloHuggingfaceAPI
    :<|> ConfigAPI

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

server :: Configuration -> Server CombinedAPI
server config =
  getRoot
    :<|> allTagsH
    :<|> allEntriesH
    :<|> allCacheH
    :<|> allTimestampsH
    :<|> postNoteH
    :<|> postCompletedH
    :<|> getCompletedH
    :<|> searchH
    :<|> frontendH
    :<|> linkEntryTagsH
    :<|> helloTorchH
    :<|> helloHuggingfaceH
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
