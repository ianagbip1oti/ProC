module ProC.Parser.BlnExpressionSpec (spec) where

import           ProC.Language
import           ProC.LanguageInstances        ()
import           ProC.Parser.BlnExpression
import           ProC.Parser.ProC

import           Test.Hspec
import           Test.QuickCheck

formatBool :: Bool -> String
formatBool True = "tru"
formatBool False = "fls"

formatBinOp :: BlnBinOp -> String
formatBinOp And = "&&"
formatBinOp Or  = "||"

spec :: Spec
spec =
  describe "term" $ do
    it "should parse literals" $ property $ \b ->
      parse blnExpression (formatBool b) == Right (BlnLiteral b)
    it "should parse binary ops" $ property $ \o l r ->
      parse blnExpression (formatBool l ++ formatBinOp o ++ formatBool r) == Right (BlnBinOp o (BlnLiteral l) (BlnLiteral r))
    it "should parse unary op" $ property $ \b ->
      parse blnExpression ("! " ++ formatBool b) == Right (BlnUnaryOp Not (BlnLiteral b))
    