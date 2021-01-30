{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import DB
import Data.Text (isInfixOf, pack, replace, unpack)
import Network.URI (URI, isURI, parseURI)
import System.Directory
import System.Process
import Text.HTML.Scalpel (Scraper, chroots, scrapeURL, text)
import Text.Pretty.Simple (pPrint)
import Text.Printf
import Data.List (sort)
import System.FilePath.Posix (takeBaseName)

-- TODO - this doesn't work yet - need to use the query format
arxivTransform :: String -> String
arxivTransform url = unpack . replace "arxiv.org" "export.arxiv.org" $ pack url

-- arxivTransform url = "http://export.arxiv.org/api/query?id_list=cs/9901002v1"

-- TODO - add other transformations
urlTransformations = arxivTransform

idURL :: String -> URLType
idURL url
  | "arxiv.org" `isInfixOf` pack url = ArxivURL
  | "twitter.com" `isInfixOf` pack url = TwitterURL
  | "pdf" `isInfixOf` pack url = PdfURL
  | otherwise = GenericURL

getTitle :: String -> IO (Maybe PageTitle)
getTitle url = fmap (\x -> if not (null x) then head x else PageTitle "") <$> scrapeURL url title
  where
    title :: Scraper String [PageTitle]
    title = do
      chroots "html" $ do
        pageTitle <- text "title"
        return $ PageTitle pageTitle

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

scrapeTitle :: String -> IO (Maybe PageTitle)
scrapeTitle url = do
  let urlType = idURL url
  case urlType of
    ArxivURL -> pure Nothing
    TwitterURL -> pure Nothing
    PdfURL -> pure Nothing
    _ -> do
      result <- getTitle url
      if result == Just (PageTitle "") then pure Nothing else pure result

newtype Timeout = Timeout Int

screenshot :: Timeout -> String -> Int -> IO ()
screenshot (Timeout timeout) url fileID = do
  createDirectoryIfMissing True "screenshots"
  fp <- findExecutable "chromium"
  let result = case fp of
        Just _ -> undefined
        Nothing -> error ""
  let outFile = mkScreenShotFilename fileID
  let args =
        [ printf "%ds" timeout,
          "chromium",
          "--headless",
          "--disable-gpu",
          "--window-size=600,800",
          "--hide-scrollbars",
          printf "--screenshot=%s" outFile,
          url
        ]
  (code, stdout, stderr) <- readProcessWithExitCode "timeout" args ""
  print code
  print $ "Writing to: " ++ outFile

cacheEntries :: IO ()
cacheEntries = do
  entries <- allEntries
  let linkEntries = filter (isURI . content) entries
  let links = urlTransformations . content <$> linkEntries
  let filt = id -- replace `id` with `take N` when debugging
  titles <-
    mapM
      ( \url -> do
          putStrLn $ "Querying url: " ++ url
          catchAny (threadDelay 50000 >> scrapeTitle url) $ \e -> do
            putStrLn $ "Got an exception: " ++ show e
            putStrLn "Returning dummy value of Nothing"
            pure $ Just $ PageTitle url
      )
      (filt links) -- for testing
  let cacheEntries = crawlerOutput2cache $ zip3 (filt linkEntries) (filt links) (filt titles)
  writeCache cacheEntries
  pPrint titles

screenshotEntries :: IO ()
screenshotEntries = do
  entries <- allEntries
  let linkEntries = filter (isURI . content) entries
  let links = zip (urlTransformations . content <$> linkEntries) (entryID <$> linkEntries)
  let filt = id -- replace `id` with `take N` when debugging
  mapM_
    ( \(url, entryid) -> do
        exists <- doesFileExist (mkScreenShotFilename entryid)
        if not exists then do
          putStrLn $ "Screenshotting url: " ++ url
          catchAny (threadDelay 50000 >> screenshot (Timeout 10) url entryid) $ \e -> do
            putStrLn $ "Got an exception: " ++ show e
            putStrLn "Returning dummy value of Nothing"
            -- pure $ Just $ PageTitle url
          else 
            putStrLn $ "Screenshot already exists for url: " ++ url
    )
    (filt links)

ocrShots :: IO ()
ocrShots = do
  createDirectoryIfMissing True "ocr"
  files <- sort <$> listDirectory "screenshots"
  mapM_ (\file -> do
    exists <- doesFileExist (ss2ocrFilename file)
    if not exists then do
      putStrLn $ "Running OCR for: " ++ file
      let args = ["10s",
                "tesseract",
                "screenshots/" ++ file, 
                ss2ocrFilename file
                 ]
      readProcessWithExitCode "timeout" args ""
      pure ()
      else 
        putStrLn $ "OCR already exists for file: " ++ ss2ocrFilename file
    ) files 

main = do
  screenshotEntries
  ocrShots
  cacheEntries
