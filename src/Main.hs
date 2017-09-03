module Main(main) where

import ProC.Language
import ProC.Parser

import Control.Applicative

import System.Environment

main :: IO ()
main = do
    inputFile <- head <$> getArgs
    input <- readFile inputFile
    case parseProC input of
      (Left e) -> putStrLn $ show e
      (Right s) -> putStrLn (show s) >> exec s
