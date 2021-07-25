{-# LANGUAGE DeriveGeneric #-}

module Date where 

import Data.Aeson (FromJSON, ToJSON)
import Text.Printf (printf)
import GHC.Generics (Generic)

data DateTime = DateTime {
  dtDay :: (Int, Int, Int),
  dtTimeOfDay :: (Int, Int, Int),
  dtUTC :: Int
} deriving (Show, Generic) 

instance ToJSON DateTime
