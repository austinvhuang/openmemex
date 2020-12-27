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

module DB where


import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import GHC.TypeLits
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

-- Helper functions

date2string year month day = printf "%.4d-%.2d-%.2d" year month day

-- Handlers

queryDate :: Int -> Int -> Int -> IO [Entry]
queryDate year month day = do
  conn <- open dbFile
  let queryString = Query $ pack $ "SELECT * FROM entries WHERE date == \"" ++ (date2string year month day) ++ "\""
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r

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

-- | Returns a unique list of all tags
-- See https://stackoverflow.com/questions/32098328/no-instance-for-database-sqlite-simple-fromfield-fromfield-char
allTags :: IO [[String]]
allTags = do
  conn <- open dbFile
  let queryString = Query $ pack $ "SELECT distinct tag from tags order by tag"
  r <- query_ conn queryString :: IO [[String]]
  close conn
  pure r

allEntries :: IO [Entry]
allEntries = do
  conn <- open dbFile
  r <- query_ conn "SELECT * from entries" :: IO [Entry]
  close conn
  pure r

queryContent :: String -> IO [Entry]
queryContent query = do
  conn <- open dbFile
  let queryString = 
       Query $ pack $ "SELECT * FROM entries WHERE content LIKE '%" ++ query  ++ "%'"
  r <- query_ conn queryString :: IO [Entry]
  close conn
  pure r
