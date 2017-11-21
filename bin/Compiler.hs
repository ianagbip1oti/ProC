module Main
  ( main
  ) where

import           ProC.Parser

import           System.Environment

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  prog <- parseProC input
  case prog of
    (Left e)  -> print e
    (Right p) -> print p
