module Main(main) where

import ProC.Interpreter
import ProC.Parser

import System.Environment

main :: IO ()
main = do
    inputFile <- head <$> getArgs
    input <- readFile inputFile
    case parseProC input of
      (Left e) -> putStrLn $ show e
      (Right p) -> run p
