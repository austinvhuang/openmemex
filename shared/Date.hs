{-# LANGUAGE DeriveGeneric #-}

module Date where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Text.Printf (printf)

data DateTime = DateTime
  { dtDay :: (Int, Int, Int),
    dtTimeOfDay :: (Int, Int, Int),
    dtUTC :: Int
  }
  deriving (Show, Generic)

instance ToJSON DateTime
