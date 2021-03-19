{-# LANGUAGE DeriveGeneric #-}
module Models where

import Torch
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


data TestTorch = TestTorch { msg :: String, value :: [Float] } deriving (Show, Generic)
instance ToJSON TestTorch
instance FromJSON TestTorch
  
runModel = do
  print $ asTensor [1.0 :: Float, 2.0] 
  pure ()

helloTorchH value = liftIO $ helloTorch value

helloTorch :: Float -> IO [TestTorch]
helloTorch value = pure $ [TestTorch "hello. f(x) = 2 * x from hasktorch." [result]]
  where 
    t = asTensor value :: Tensor
    result = asValue (2.0 * t) :: Float
