{-# LANGUAGE OverloadedStrings #-}

module Files where

import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

mkScreenshotFilename :: Int -> String
mkScreenshotFilename = printf "screenshots/%.10d.png"

mkThumbnailFilename :: Int -> String
mkThumbnailFilename = printf "thumbnails/%.10d_tn.png"

mkOCRFilename :: Int -> String
mkOCRFilename = printf "ocr/%.10d.txt"

ss2ocrFilename :: String -> String
ss2ocrFilename x = "ocr/" ++ takeBaseName x ++ ".txt"

ss2ocrPrefix :: String -> String
ss2ocrPrefix x = "ocr/" ++ takeBaseName x

ocr2ssFilename :: String -> String
ocr2ssFilename x = "screenshots/" ++ takeBaseName x ++ ".png"
