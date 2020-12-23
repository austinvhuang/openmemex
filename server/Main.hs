{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


import Data.Aeson
import GHC.Generics
import GHC.TypeLits

import Servant
import Network.Wai.Handler.Warp
import System.IO
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
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
  { 
    tagID :: Maybe Int, 
    foreignID :: Int,
    tag :: String
  }
  deriving (Show, Generic)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

instance ToJSON Tag

type RootAPI = Get '[JSON] [String]
type EntryAPI = "date" :> Capture "year" Int :> Capture "month" Int :> Capture "day" Int :> Get '[JSON] [Entry]
type CombinedAPI = RootAPI :<|> EntryAPI

server :: Server CombinedAPI
server = getRoot :<|> getEntries

getRoot :: Handler [String]
getRoot = return [ "hello" ]

entries = [ Entry 123 "2020-11-21" "4:00pm" "hello world",
  Entry 124 "2020-11-21" "5:00pm" "hello foo" ,
  Entry 125 "2020-11-24" "5:00pm" "hello foo"] :: [Entry]

getEntries :: Int -> Int -> Int -> Handler [Entry]
getEntries year month day = return matches
  where
    matches = filter (\x -> (date x) == printf "%.4d-%.2d-%.2d" year month day) entries 

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

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
