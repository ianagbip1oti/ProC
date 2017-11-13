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
  it "should support reference variables in outer scope" $
    run [r| int a=3; { print(tos(a)); } |] `shouldReturn` "3\n"
  it "should support shadowing in blocks" $
    run
      [r|
        int a=3;
        print(tos(a));
        {
          int a=4;
          print(tos(a));
        }
        print(tos(a));
      |] `shouldReturn`
    "3\n4\n3\n"
  it "supports while loops" $
    run
      [r|
        int a = 0;
        print ("a");
        whl (a > 0) {
          print("b");
        }
        print("c");
      |] `shouldReturn`
    "a\nc\n"
