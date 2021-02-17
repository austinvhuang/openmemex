#!/usr/bin/env stack
-- stack --resolver lts-17.4 script

module Main where

import System.Directory
import System.FilePath.Posix (takeBaseName)
import System.Process

main = do
  files <- listDirectory "screenshots"
  mapM_ ( \file -> do
    let outFile = (takeBaseName file) ++ "_tn.png"
    let args = ["-resize", "50%", "screenshots/" ++ file, "screenshots/" ++ outFile]
    putStrLn file
    putStrLn outFile
    (code, stdout, stderr) <- readProcessWithExitCode "convert" args ""
    pure ()
    ) files
  putStrLn "Done"
