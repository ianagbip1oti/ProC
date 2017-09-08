module ProC.Parser.StringExpressionSpec where

import ProC.Language
import ProC.Parser.ProC
import ProC.Parser.StringExpression

import Test.Hspec

spec :: Spec
spec = do
  describe "term" $ do
    it "parses simple string" $ do
      parse stringExpression "\"a string\"" `shouldBe` Right (StringLiteral "a string")
      
