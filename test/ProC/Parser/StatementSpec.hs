module ProC.Parser.StatementSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser.ProC
import           ProC.Parser.Statement

import           Test.Hspec

spec :: Spec
spec =
  describe "statement" $ do
    it "should parse int var decl" $
      parse statement "int a = 1" `shouldBe`
      Right (IntVarDecl (Identifier "a") (IntLiteral 1))
    it "should parse string var decl" $
      parse statement "str a = \"abc\"" `shouldBe`
      Right (StrVarDecl (Identifier "a") (StrLiteral "abc"))
