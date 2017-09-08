module ProC.Parser.NumericExpressionSpec where

import ProC.Language
import ProC.Parser.NumericExpression
import ProC.Parser.ProC

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  describe "term" $ do
    it "should parse positive int literals" $ property $ do
      \i -> i > 0 ==> parse numericExpression (show i) == Right (IntLiteral i)
    it "should parse negative int literals" $ property $ do
      \i -> i < 0 ==> parse numericExpression (show i) == Right (UnaryOp Negate (IntLiteral (-i)))
    
    