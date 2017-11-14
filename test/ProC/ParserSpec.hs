{-# LANGUAGE QuasiQuotes #-}

module ProC.ParserSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser

import           Test.Hspec

import           Text.RawString.QQ

spec :: Spec
spec =
  describe "parseProC" $ do
    it "parses Hello World" $
      parseProC [r| print("Hello World"); |] `shouldReturn`
      Right (Seq [Print (PStrLiteral "Hello World")])
    it "parses assignment" $
      parseProC [r| int a=1; a=2; |] `shouldReturn`
      Right
        (Seq
           [ PIntVarDcl (Identifier "a") (PIntLiteral 1)
           , PIntVarAss (Identifier "a") (PIntLiteral 2)
           ])
    it "parses blocks" $
      parseProC [r| { print("Hello World"); } |] `shouldReturn`
      Right (Seq [Block [Print (PStrLiteral "Hello World")]])
    it "allows references to outer scopes" $
      parseProC [r| int a=0; { int b=a; } |] `shouldReturn`
      Right
        (Seq
           [ PIntVarDcl (Identifier "a") (PIntLiteral 0)
           , Block [PIntVarDcl (Identifier "b") (PIntVariable (Identifier "a"))]
           ])
    it "allows shadowing in blocks" $
      parseProC [r| int a=0; { int a=1; } |] `shouldReturn`
      Right
        (Seq
           [ PIntVarDcl (Identifier "a") (PIntLiteral 0)
           , Block [PIntVarDcl (Identifier "a") (PIntLiteral 1)]
           ])
    it "allows statements after blocks" $
      parseProC [r| { int a=1; } print("Hello World"); |] `shouldReturn`
      Right
        (Seq
           [ Block [PIntVarDcl (Identifier "a") (PIntLiteral 1)]
           , Print (PStrLiteral "Hello World")
           ])
    it "allows shadowing after blocks" $
      parseProC [r| { int a=1; } int a=0; |] `shouldReturn`
      Right
        (Seq
           [ Block [PIntVarDcl (Identifier "a") (PIntLiteral 1)]
           , PIntVarDcl (Identifier "a") (PIntLiteral 0)
           ])
    it "parses whl loops" $
      parseProC [r| whl (tru) { print("Hello World"); } |] `shouldReturn`
      Right (Seq [Whl (PBlnLiteral True) [Print (PStrLiteral "Hello World")]])
