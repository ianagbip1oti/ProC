module ProC.Parser.StringExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser.ProC
import           ProC.Parser.StringExpression

import           Test.Hspec

spec :: Spec
spec =
  describe "term" $
  it "parses simple string" $
  parse stringExpression "\"a string\"" `shouldBe` Right (StrLiteral "a string")
