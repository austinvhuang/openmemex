{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import CrawlTools
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

main = crawlAll
