{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Text.Printf (printf)

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

data Tag = Tag
  { tagID :: Maybe Int,
    foreignID :: Int,
    tag :: String
  }
  deriving (Show, Generic)

data Date = Date
  { year :: String,
    month :: String,
    day :: String
  }
  deriving (Show, Generic)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

instance ToJSON Tag

dbFile = "note2self.db"

-- API Types

type RootAPI = Get '[JSON] [String]

type DateAPI = "date" :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Get '[JSON] [Entry]

type RangeAPI = "range" :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Get '[JSON] [Entry]

type AllTagsAPI = "all" :> "tags" :> Get '[JSON] [[String]]

type AllEntriesAPI = "all" :> "entries" :> Get '[JSON] [Entry]

type CombinedAPI = RootAPI :<|> DateAPI :<|> RangeAPI :<|> AllTagsAPI :<|> AllEntriesAPI

-- for testing
entries =
  [ Entry 123 "2020-11-21" "4:00pm" "hello world",
    Entry 124 "2020-11-21" "5:00pm" "hello foo",
    Entry 125 "2020-11-24" "5:00pm" "hello foo"
  ] ::
    [Entry]

date2string year month day = printf "%.4d-%.2d-%.2d" year month day

-- Handlers

getRoot :: Handler [String]
getRoot = return ["n2s API"]

queryDate :: Int -> Int -> Int -> IO [Entry]
queryDate year month day = do
  conn <- open dbFile
  let queryString = Query $ pack $ "SELECT * FROM entries WHERE date == \"" ++ (date2string year month day) ++ "\""
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r

queryDateH y m d = liftIO $ queryDate y m d :: Handler [Entry]

queryRange :: Int -> Int -> Int -> Int -> Int -> Int -> IO [Entry]
queryRange startYear startMonth startDay endYear endMonth endDay = do
  conn <- open dbFile
  let queryString =
        Query $ pack $ "SELECT * FROM entries WHERE date BETWEEN \"" 
        ++ date2string startYear startMonth startDay 
        ++ "\" AND \"" 
        ++ date2string endYear endMonth endDay ++ "\""
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r

queryRangeH y1 m1 d1 y2 m2 d2 = (liftIO $ queryRange y1 m1 d1 y2 m2 d2) :: Handler [Entry]

-- | Returns a unique list of all tags
-- See https://stackoverflow.com/questions/32098328/no-instance-for-database-sqlite-simple-fromfield-fromfield-char
-- Re why this isn't [String]
allTags :: IO [[String]]
allTags = do
  conn <- open dbFile
  let queryString = Query $ pack $ "SELECT distinct tag from tags order by tag"
  r <- query_ conn queryString :: IO [[String]]
  close conn
  pure r

allTagsH = liftIO allTags :: Handler [[String]]

allEntries :: IO [Entry]
allEntries = do
  conn <- open dbFile
  r <- query_ conn "SELECT * from entries" :: IO [Entry]
  close conn
  pure r

allEntriesH = liftIO allEntries :: Handler [Entry]

-- | Test function
getEntries :: Int -> Int -> Int -> Handler [Entry]
getEntries year month day = return matches
  where
    matches = filter (\x -> (date x) == printf "%.4d-%.2d-%.2d" year month day) entries

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

server :: Server CombinedAPI
server = getRoot :<|> queryDateH :<|> queryRangeH :<|> allTagsH :<|> allEntriesH

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
