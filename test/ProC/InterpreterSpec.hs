{-# LANGUAGE QuasiQuotes #-}

module ProC.InterpreterSpec
  ( spec
  ) where

import           ProC.Interpreter
import           ProC.Parser

import           System.IO.Silently

import           Test.Hspec

import           Text.RawString.QQ

run :: String -> IO String
run p = do
  parsed <- either failedParse return $ parseProC p
  capture_ $ runProC parsed
  where
    failedParse err = fail $ "Failed to parse: " ++ show err

spec :: Spec
spec = do
  it "should run hello world" $
    run [r| print("Hello World"); |] `shouldReturn` "Hello World\n"
  it "should follow order of operations" $
    run [r| print(tos(2 + 2 * 3)); |] `shouldReturn` "8\n"
