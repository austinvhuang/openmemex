{-# LANGUAGE OverloadedStrings #-}

module OCR where

import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

mkOCRFilename :: Int -> String
mkOCRFilename = printf "ocr/%.10d.txt"

ss2ocrFilename :: String -> String
ss2ocrFilename x = "ocr/" ++ takeBaseName x ++ ".txt"

ss2ocrPrefix :: String -> String
ss2ocrPrefix x = "ocr/" ++ takeBaseName x

ocr2ssFilename :: String -> String
ocr2ssFilename x = "screenshots/" ++ takeBaseName x ++ ".png"
