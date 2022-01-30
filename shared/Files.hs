{-# LANGUAGE OverloadedStrings #-}

module Files where

import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

mkScreenshotFilename :: Int -> String
mkScreenshotFilename = printf "screenshots/%.10d.png"

mkThumbnailFilename :: Int -> String
mkThumbnailFilename = printf "thumbnails/%.10d_tn.png"
