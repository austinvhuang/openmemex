module Main where

import Network.URI (URI, isURI, parseURI)
import DB

main :: IO ()
main = do
  print $ isURI "http://www.google.com"
  print $ isURI "hello world"
  entries <- allEntries
  -- mapM_ (putStrLn . show) entries
  let linkEntries = filter (isURI . content) entries
  mapM_ (putStrLn . show . content) linkEntries
