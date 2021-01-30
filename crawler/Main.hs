{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import DB
import Data.Text (isInfixOf, pack, replace, unpack)
import Network.URI (URI, isURI, parseURI)
import Text.HTML.Scalpel (Scraper, chroots, scrapeURL, text)
import Text.Pretty.Simple (pPrint)

import System.Directory
import System.Process
import Text.Printf


-- TODO - this doesn't work yet - need to use the query format
arxivTransform :: String -> String
arxivTransform url = unpack . replace "arxiv.org" "export.arxiv.org" $ pack url

-- arxivTransform url = "http://export.arxiv.org/api/query?id_list=cs/9901002v1"

-- TODO - add other transformations
urlTransformations = arxivTransform

idURL :: String -> URLType
idURL url
  | "arxiv.org" `isInfixOf` (pack url) = ArxivURL
  | "twitter.com" `isInfixOf` (pack url) = TwitterURL
  | "pdf" `isInfixOf` (pack url) = PdfURL
  | otherwise = GenericURL

getTitle :: String -> IO (Maybe PageTitle)
getTitle url = fmap (\x -> if not (null x) then head x else PageTitle "") <$> (scrapeURL url title)
  where
    title :: Scraper String [PageTitle]
    title = do
      chroots "html" $ do
        pageTitle <- text "title"
        return $ PageTitle pageTitle

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

router :: String -> IO (Maybe PageTitle)
router url = do
  let urlType = idURL url
  case urlType of
    ArxivURL -> pure Nothing
    TwitterURL -> pure Nothing
    PdfURL -> pure Nothing
    _ -> do
      result <- getTitle url
      if result == Just (PageTitle "") then pure Nothing else pure result

screenshot :: String -> Int -> IO ()
screenshot url fileID = do
  createDirectoryIfMissing True "screenshots"
  fp <- findExecutable "chromium"
  let result = case fp of 
                Just _ -> undefined
                Nothing -> error ""
  let outFile = printf "%.10d.png" fileID
  let command =
        printf "chromium --headless --disable-gpu --screenshot=%s %s" outFile url :: String
  (code, stdout, stderr) <- readProcessWithExitCode command [] ""
  print $ "Writing to " ++ outFile
  pure ()

main :: IO ()
main = do
  entries <- allEntries
  -- mapM_ (putStrLn . show) entries
  let linkEntries = filter (isURI . content) entries
  let links = urlTransformations <$> content <$> linkEntries
  let filt = id
  -- let filt = take 5 -- for debugging

  titles <-
    mapM
      ( \url -> do
          putStrLn $ "Querying url: " ++ url
          titles <- catchAny (threadDelay 100000 >> router url) $ \e -> do
            putStrLn $ "Got an exception: " ++ show e
            putStrLn "Returning dummy value of Nothing"
            pure $ Just $ PageTitle url
          pure titles
      )
      (filt links) -- for testing
  let cacheEntries = crawlerOutput2cache $ zip3 (filt linkEntries) (filt links) (filt titles)
  writeCache cacheEntries
  pPrint titles
