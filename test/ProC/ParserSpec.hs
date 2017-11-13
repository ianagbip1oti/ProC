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
      parseProC [r| print("Hello World"); |] `shouldBe`
      Right (Seq [Print (PStrLiteral "Hello World")])
    it "parses blocks" $
      parseProC [r| { print("Hello World"); } |] `shouldBe`
      Right (Seq [Block [Print (PStrLiteral "Hello World")]])
    it "allows references to outer scopes" $
      parseProC [r| int a=0; { int b=a; } |] `shouldBe`
      Right
        (Seq
           [ IntVarDcl (Identifier "a") (PIntLiteral 0)
           , Block [IntVarDcl (Identifier "b") (PIntVariable (Identifier "a"))]
           ])
    it "allows shadowing in blocks" $
      parseProC [r| int a=0; { int a=1; } |] `shouldBe`
      Right
        (Seq
           [ IntVarDcl (Identifier "a") (PIntLiteral 0)
           , Block [IntVarDcl (Identifier "a") (PIntLiteral 1)]
           ])
    it "allows statements after blocks" $
      parseProC [r| { int a=1; } print("Hello World"); |] `shouldBe`
      Right
        (Seq
           [ Block [IntVarDcl (Identifier "a") (PIntLiteral 1)]
           , Print (PStrLiteral "Hello World")
           ])
    it "allows shadowing after blocks" $
      parseProC [r| { int a=1; } int a=0; |] `shouldBe`
      Right
        (Seq
           [ Block [IntVarDcl (Identifier "a") (PIntLiteral 1)]
           , IntVarDcl (Identifier "a") (PIntLiteral 0)
           ])
