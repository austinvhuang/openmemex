{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import DB
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Text (isInfixOf, isSuffixOf, pack, replace, unpack)
import Network.URI (URI, isURI, parseURI)
import OCR
import System.Directory
import System.FilePath.Posix (takeBaseName)
import System.Process
import Text.HTML.Scalpel (Scraper, chroots, scrapeURL, text)
import Text.Pretty.Simple (pPrint)
import Text.Printf
import Text.Read (readMaybe)

import CrawlTools

cacheAllEntries :: IO ()
cacheAllEntries = do
  allEntries <- allEntries
  cacheEntries allEntries

screenshotEntries :: Bool -> Timeout -> IO ()
screenshotEntries deltaOnly timeout = do
  entries <- allEntries
  let linkEntries = filter (isURI . content) entries
  let links = zip (urlTransformations . content <$> linkEntries) (entryID <$> linkEntries)
  mapM_
    ( \(url, entryid) -> do
        exists <- doesFileExist (mkScreenshotFilename entryid)
        if (not deltaOnly || not exists) && (idURL url /= PdfURL)
          then do
            putStrLn $ "Screenshotting url: " ++ url
            catchAny (threadDelay 50000 >> screenshot timeout url entryid) $ \e -> do
              putStrLn $ "Got an exception: " ++ show e
              putStrLn "Returning dummy value of Nothing"
          else -- pure $ Just $ PageTitle url)

            putStrLn $ "Screenshot already exists or is not possible (eg a pdf) for url: " ++ url
    )
    (filt links)

ocrShots :: IO ()
ocrShots = do
  -- make ocr output files from screenshots
  createDirectoryIfMissing True "ocr"
  files <- sort <$> listDirectory "screenshots"
  mapM_
    ( \file -> do
        exists <- doesFileExist (ss2ocrFilename file)
        if not exists
          then do
            putStrLn $ "Running OCR for: " ++ file
            let args =
                  [ "10s",
                    "tesseract",
                    "screenshots/" ++ file,
                    ss2ocrPrefix file
                  ]
            readProcessWithExitCode "timeout" args ""
            pure ()
          else putStrLn $ "OCR already exists for file: " ++ ss2ocrFilename file
    )
    files
  -- read ocr content from file
  ocrFiles <- sort <$> listDirectory "ocr"
  ocrEntries <-
    mapM
      ( \file -> do
          content <- readFile $ "ocr/" ++ file
          let entryid = readMaybe (takeBaseName file) :: Maybe Int
          case entryid of
            Just entryid -> pure $ Just (OCREntry entryid (ocr2ssFilename file) content)
            Nothing -> pure Nothing
      )
      ocrFiles
  -- write entries to database
  writeOCR (catMaybes ocrEntries)

thumbnails = do
  files <- listDirectory "screenshots"
  mapM_ ( \file -> do
    let outFile = (takeBaseName file) ++ "_tn.png"
    createDirectoryIfMissing True "thumbnails"
    let args = ["-resize", "30%", "screenshots/" ++ file, "thumbnails/" ++ outFile]
    putStrLn file
    putStrLn outFile
    (code, stdout, stderr) <- readProcessWithExitCode "convert" args ""
    pure ()
    ) files

main = do

  screenshotEntries True (Timeout 30) -- False to reconstruct screenshots directory
  thumbnails
  ocrShots
  -- crawl pages
  cacheAllEntries
