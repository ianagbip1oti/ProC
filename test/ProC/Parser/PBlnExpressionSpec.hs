module ProC.Parser.PBlnExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.LanguageInstances    ()
import           ProC.Parser.PBlnExpression
import           ProC.Parser.ProC

import           Test.Hspec
import           Test.QuickCheck

formatBool :: Bool -> String
formatBool True  = "tru"
formatBool False = "fls"

formatBinOp :: PBlnBinOpr -> String
formatBinOp And = "&&"
formatBinOp Or  = "||"

spec :: Spec
spec =
  describe "term" $ do
    it "should parse literals" $
      property $ \b -> parse pBlnExpression (formatBool b) == Right (PBlnLiteral b)
    it "should parse binary ops" $
      property $ \o l r ->
        parse pBlnExpression (formatBool l ++ formatBinOp o ++ formatBool r) ==
        Right (PBlnBinOpr o (PBlnLiteral l) (PBlnLiteral r))
    it "should parse unary op" $
      property $ \b ->
        parse pBlnExpression ("! " ++ formatBool b) ==
        Right (PBlnUnrOpr Not (PBlnLiteral b))
