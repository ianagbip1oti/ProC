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
  describe "intVarDeclStatement" $ do
    it "should parse int var decl" $
      parse statement "int a = 1" `shouldBe`
      Right (IntVarDecl (Identifier "a") (Literal 1))
    it "should fail two vars same name" $
      parse statements "int a = 1; int a = 1;" `shouldSatisfy` isLeft
  describe "strVarDeclStatement" $ do
    it "should parse string var decl" $
      parse statement "str a = \"abc\"" `shouldBe`
      Right (StrVarDecl (Identifier "a") (StrLiteral "abc"))
    it "should fail two vars same name" $
      parse statements "str a = \"a\"; str a = \"b\";" `shouldSatisfy` isLeft
