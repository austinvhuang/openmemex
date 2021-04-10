{-# LANGUAGE DeriveGeneric #-}

module Models where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Torch
import Tokenizers

data TestTorch = TestTorch {msg :: String, value :: [Float]} deriving (Show, Generic)
instance ToJSON TestTorch
instance FromJSON TestTorch

data TestHuggingface = TestHuggingface {hfToken :: String, hfResult :: [Int]} deriving (Show, Generic)
instance ToJSON TestHuggingface
instance FromJSON TestHuggingface

runModel = do
  print $ asTensor [1.0 :: Float, 2.0]
  pure ()

helloTorchH value = liftIO $ helloTorch value

helloTorch :: Float -> IO [TestTorch]
helloTorch value = pure $ [TestTorch "hello. f(x) = 2 * x from hasktorch." [result]]
  where
    t = asTensor value :: Tensor
    result = asValue (2.0 * t) :: Float

helloHuggingfaceH value = liftIO $ helloHuggingface value

hface string tokenizer = do
  encoding <- encode tokenizer string
  -- tokens <- getTokens encoding
  getIDs encoding

helloHuggingface :: String -> IO [TestHuggingface]
helloHuggingface value = do
  tokenizer <- mkRobertaTokenizer "roberta-base-vocab.json" "roberta-base-merges.txt"
  result <- hface value tokenizer
  pure [TestHuggingface value [result !! 0]]
