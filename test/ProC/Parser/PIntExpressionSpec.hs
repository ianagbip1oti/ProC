module ProC.Parser.PIntExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.LanguageInstances        ()
import           ProC.Parser.PIntExpression
import           ProC.Parser.ProC
import           ProC.Parser.Statement

import           Data.Either

import           Test.Hspec
import           Test.QuickCheck

formatBinOp :: PIntBinOpr -> String
formatBinOp Add      = "+"
formatBinOp Subtract = "-"
formatBinOp Multiply = "*"
formatBinOp Divide   = "/"

one :: PIntExpression
one = PIntLiteral 1

spec :: Spec
spec =
  describe "term" $ do
    it "should parse positive int literals" $ property $ \i ->
      i > 0 ==> parse pIntExpression (show i) == Right (PIntLiteral i)
    it "should parse negative int literals" $ property $ \i ->
      i < 0 ==> parse pIntExpression (show i) ==
      Right (PIntUnrOpr Negate (PIntLiteral (-i)))
    it "should parse binary ops" $ property $ \op ->
      parse pIntExpression ("1 " ++ formatBinOp op ++ " 1") `shouldBe`
      Right (PIntBinOpr op one one)
    it "should parse int variables" $ parse statements "int a=1; int b=a*1+a;" `shouldSatisfy`
      isRight
    it "shold fail with str variable" $
      parse statements "str a=\"abc\"; int b=a*1+a;" `shouldSatisfy`
      isLeft
