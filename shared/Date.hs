{-# LANGUAGE DeriveGeneric #-}

module Date where 

import Data.Aeson (FromJSON, ToJSON)
import Text.Printf (printf)
import GHC.Generics (Generic)

data Date = Date {
  year :: Int,
  month :: Int,
  day :: Int
} deriving (Show, Generic)

instance ToJSON Date
instance FromJSON Date

data DateWindow = DateWindow {
  twStart :: Date,
  twEnd :: Date
} deriving (Show, Generic)

instance ToJSON DateWindow
instance FromJSON DateWindow

-- | date2string year month day
date2string :: Int -> Int -> Int -> String
date2string = printf "%.4d-%.2d-%.2d"

data DateTime = DateTime {
  dtDay :: (Int, Int, Int),
  dtTimeOfDay :: (Int, Int, Int),
  dtUTC :: Int
} deriving (Show, Generic) 

instance ToJSON DateTime
