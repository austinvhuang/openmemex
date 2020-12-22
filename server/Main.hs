{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}


import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant.API

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Entry = Entry
  { entryID :: Int, 
    date :: String,
    time :: String,
    content :: String
  }
  deriving (Show)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field

data Tag = Tag
  { 
    tagID :: Maybe Int, 
    foreignID :: Int,
    tag :: String
  }
  deriving (Show)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

type EntryAPI = "entries" :> QueryParam "date" String

type RootEndpoint = Get '[JSON] Entry

main = do
  putStrLn "Running"
