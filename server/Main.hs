{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import DB
import Data.Text (Text, pack, unpack)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setPort,
  )
import Servant
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- API Types

type RootAPI = Get '[JSON] [String]

type DateAPI =
  "date" :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Get '[JSON] [Entry]

type RangeAPI =
  "range" :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int
    :> Capture "year" Int
    :> Capture "month" Int
    :> Capture "day" Int
    :> Get '[JSON] [Entry]

type AllTagsAPI = "all" :> "tags" :> Get '[JSON] [[String]]

type AllEntriesAPI = "all" :> "entries" :> Get '[JSON] [Entry]

type AllCacheAPI = "all" :> "cache" :> Get '[JSON] [CacheView]

type ContentAPI = "content" :> Capture "query" String :> Get '[JSON] [Entry]

type CombinedAPI =
  RootAPI :<|> DateAPI :<|> RangeAPI :<|> AllTagsAPI :<|> AllEntriesAPI :<|> AllCacheAPI :<|> ContentAPI

-- Helper functions

date2string year month day = printf "%.4d-%.2d-%.2d" year month day

-- Handlers

getRoot :: Handler [String]
getRoot = return ["n2s API"]

queryDateH y m d = liftIO $ queryDate y m d :: Handler [Entry]

queryRangeH :: Int -> Int -> Int -> Int -> Int -> Int -> Handler [Entry]
queryRangeH y1 m1 d1 y2 m2 d2 = (liftIO $ queryRange y1 m1 d1 y2 m2 d2) :: Handler [Entry]

allTagsH = liftIO allTags :: Handler [[String]]

allEntriesH = liftIO allEntries :: Handler [Entry]

allCacheH = liftIO allCache :: Handler [CacheView]

queryContentH q = liftIO $ queryContent q :: Handler [Entry]

-- App definition

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

server :: Server CombinedAPI
server = getRoot :<|> queryDateH :<|> queryRangeH :<|> allTagsH :<|> allEntriesH :<|> allCacheH :<|> queryContentH

mkApp :: IO Application
mkApp = return $ serve combinedApi server

runs :: IO ()
runs = do
  let port = 3000
      settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
            defaultSettings
  runSettings settings =<< mkApp

main = runs
