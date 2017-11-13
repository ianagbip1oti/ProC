module ProC.Parser.PStrExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser.ProC
import           ProC.Parser.Statement
import           ProC.Parser.PStrExpression

import           Data.Either

import           Test.Hspec

spec :: Spec
spec =
  describe "term" $ do
    it "parses simple string" $
      parse pStrExpression "\"a string\"" `shouldBe`
      Right (PStrLiteral "a string")
    it "should parse str variables" $
      parse statements "str a=\"abc\"; str b=a++\"def\";" `shouldSatisfy`
      isRight
    it "shold fail with int variable" $
      parse statements "int a=1; str b=a++\"def\";" `shouldSatisfy` isLeft