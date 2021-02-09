{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import DB
import Data.Text (isInfixOf, isSuffixOf, pack, replace, unpack)
import Network.URI (URI, isURI, parseURI)
import System.Directory
import System.Process
import Text.HTML.Scalpel (Scraper, chroots, scrapeURL, text)
import Text.Pretty.Simple (pPrint)
import Text.Printf
import Data.List (sort)
import System.FilePath.Posix (takeBaseName)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

-- TODO - this doesn't work yet - need to use the query format
arxivTransform :: String -> String
arxivTransform url = unpack . replace "arxiv.org" "export.arxiv.org" $ pack url

-- arxivTransform url = "http://export.arxiv.org/api/query?id_list=cs/9901002v1"

-- TODO - add other transformations
urlTransformations = arxivTransform

idURL :: String -> URLType
idURL url
  | "pdf" `isInfixOf` pack url = PdfURL -- order here should come before arxivurl to take precedence
  | "arxiv.org" `isInfixOf` pack url = ArxivURL
  | "twitter.com" `isInfixOf` pack url = TwitterURL
  | otherwise = GenericURL

parsePage :: String -> IO (Maybe WebPage)
parsePage url = fmap (\x -> if not (null x) then head x else WebPage "" "") <$> scrapeURL url title
  where
    title :: Scraper String [WebPage]
    title = do
      chroots "html" $ do
        title <- text "title"
        body <- text "body"
        return $ WebPage title body

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

scrapePage :: String -> IO (Maybe WebPage)
scrapePage url = do
  let urlType = idURL url
  case urlType of
    ArxivURL -> pure $ Just (WebPage url "")
    TwitterURL -> pure $ Just (WebPage url "")
    PdfURL -> pure $ Just (WebPage url "")
    _ -> do
      result <- parsePage url
      if result == Just (WebPage "" "") then pure Nothing else pure result

newtype Timeout = Timeout Int

screenshot :: Timeout -> String -> Int -> IO ()
screenshot (Timeout timeout) url fileID = do
  createDirectoryIfMissing True "screenshots"
  fp <- findExecutable "chromium"
  let result = case fp of
        Just _ -> undefined
        Nothing -> error ""
  let outFile = mkScreenshotFilename fileID
  let args =
        [ printf "%ds" timeout,
          "chromium",
          "--headless",
          "--incognito",
          "--run-all-compositor-stages-before-draw",
          "--virtual-time-budget=30000",
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
  pages <-
    mapM
      ( \url -> do
          putStrLn $ "Querying url: " ++ url
          catchAny (threadDelay 50000 >> scrapePage url) $ \e -> do
            putStrLn $ "Got an exception: " ++ show e
            putStrLn "Returning dummy value of Nothing"
            pure $ Just $ WebPage url ""
      )
      (filt links) -- for testing
  let cacheEntries = crawlerOutput2cache $ zip3 (filt linkEntries) (filt links) (filt pages)
  writeCache cacheEntries
  pPrint pages

screenshotEntries :: Bool -> IO ()
screenshotEntries deltaOnly = do
  entries <- allEntries
  let linkEntries = filter (isURI . content) entries
  let links = zip (urlTransformations . content <$> linkEntries) (entryID <$> linkEntries)
  let filt = id -- replace `id` with `take N` when debugging
  mapM_
    ( \(url, entryid) -> do
        exists <- doesFileExist (mkScreenshotFilename entryid)
        if (not deltaOnly || not exists) && (idURL url /= PdfURL) then do
          putStrLn $ "Screenshotting url: " ++ url
          catchAny (threadDelay 50000 >> screenshot (Timeout 30) url entryid) $ \e -> do
            putStrLn $ "Got an exception: " ++ show e
            putStrLn "Returning dummy value of Nothing"
            -- pure $ Just $ PageTitle url
          else 
            putStrLn $ "Screenshot already exists or is not possible (eg a pdf) for url: " ++ url
    )
    (filt links)

ocrShots :: IO ()
ocrShots = do
  createDirectoryIfMissing True "ocr"
  -- files <- fmap (\x -> "screenshots/" ++ x) (sort <$> listDirectory "screenshots")
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
  ocrFiles <- sort <$> listDirectory "ocr"
  ocrEntries <- mapM (\file -> do
      content <- readFile $ "ocr/" ++ file
      let entryid = readMaybe (takeBaseName file) :: Maybe Int
      case entryid of
        Just entryid -> pure $ Just (OCREntry entryid (ocr2ssFilename file) content)
        Nothing -> pure Nothing
    ) ocrFiles
  writeOCR (catMaybes ocrEntries) 


main = do
  screenshotEntries True -- False to reconstruct screenshots directory
  ocrShots
  cacheEntries
