module ProC.Parser.StatementSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser.ProC
import           ProC.Parser.Statement

import           Data.Either

import           Test.Hspec

spec :: Spec
spec = do
  describe "intVarDelStatement" $ do
    it "should parse int var dcl" $
      parse statement "int a = 1" `shouldBe`
      Right (PIntVarDcl (Identifier "a") (PIntLiteral 1))
    it "should fail two vars same name" $
      parse statements "int a = 1; int a = 1;" `shouldSatisfy` isLeft
  describe "strVarDclStatement" $ do
    it "should parse string var dcl" $
      parse statement "str a = \"abc\"" `shouldBe`
      Right (PStrVarDcl (Identifier "a") (PStrLiteral "abc"))
    it "should fail two vars same name" $
      parse statements "str a = \"a\"; str a = \"b\";" `shouldSatisfy` isLeft
