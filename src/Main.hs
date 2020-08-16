module Main where

import Options.Applicative

import Backend
import Parser

main :: IO ()
main = do
  initDB
  options <- execParser optionsParser
  print (options :: CommandLine)
  if (resetDB options)
    then putStrLn "Resetting DB" >> initDB
    else pure ()
  dump
  pure ()
