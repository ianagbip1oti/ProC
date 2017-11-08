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
      Right (Seq [Print (StrLiteral "Hello World")])
    it "parses blocks" $
      parseProC [r| { print("Hello World"); } |] `shouldBe`
      Right (Seq [Block [Print (StrLiteral "Hello World")]])
    it "allows references to outer scopes" $
      parseProC [r| int a=0; { int b=a; } |] `shouldBe`
      Right
        (Seq
           [ IntVarDecl (PVar (Identifier "a")) (Literal 0)
           , Block
               [ IntVarDecl
                   (PVar (Identifier "b"))
                   (Variable (PVar (Identifier "a")))
               ]
           ])
    it "allows shadowing in blocks" $
      parseProC [r| int a=0; { int a=1; } |] `shouldBe`
      Right
        (Seq
           [ IntVarDecl (PVar (Identifier "a")) (Literal 0)
           , Block [IntVarDecl (PVar (Identifier "a")) (Literal 1)]
           ])
    it "allows statements after blocks" $
      parseProC [r| { int a=1; } print("Hello World"); |] `shouldBe`
      Right
        (Seq
           [ Block [IntVarDecl (PVar (Identifier "a")) (Literal 1)]
           , Print (StrLiteral "Hello World")
           ])
    it "allows shadowing after blocks" $
      parseProC [r| { int a=1; } int a=0; |] `shouldBe`
      Right
        (Seq
           [ Block [IntVarDecl (PVar (Identifier "a")) (Literal 1)]
           , IntVarDecl (PVar (Identifier "a")) (Literal 0)
           ])
