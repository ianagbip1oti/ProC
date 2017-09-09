module ProC.Parser.NumericExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser.NumericExpression
import           ProC.Parser.ProC

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary NumericBinOp where
  arbitrary = elements [Add, Subtract, Multiply, Divide]

formatBinOp :: NumericBinOp -> String
formatBinOp Add      = "+"
formatBinOp Subtract = "-"
formatBinOp Multiply = "*"
formatBinOp Divide   = "/"

one :: NumericExpression
one = IntLiteral 1

spec :: Spec
spec =
  describe "term" $ do
    it "should parse positive int literals" $ property $ \i ->
      i > 0 ==> parse numericExpression (show i) == Right (IntLiteral i)
    it "should parse negative int literals" $ property $ \i ->
      i < 0 ==> parse numericExpression (show i) ==
      Right (UnaryOp Negate (IntLiteral (-i)))
    it "should parse binary ops" $ property $ \op ->
      parse numericExpression ("1 " ++ formatBinOp op ++ " 1") `shouldBe`
      Right (BinOp op one one)
