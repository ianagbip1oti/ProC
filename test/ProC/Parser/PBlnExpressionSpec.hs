{-# LANGUAGE QuasiQuotes #-}

module ProC.Parser.PBlnExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.LanguageInstances     ()
import           ProC.Parser.PBlnExpression
import           ProC.Parser.ProC

import           Test.Hspec
import           Test.QuickCheck

import           Text.RawString.QQ

formatBool :: Bool -> String
formatBool True  = "tru"
formatBool False = "fls"

formatBinOp :: PBlnBinOpr -> String
formatBinOp And = "&&"
formatBinOp Or  = "||"

spec :: Spec
spec =
  describe "pBlnExpression" $ do
    it "should parse literals" $
      property $ \b ->
        parse pBlnExpression (formatBool b) == Right (PBlnLiteral b)
    it "should parse binary ops" $
      property $ \o lhs rhs ->
        parse pBlnExpression (formatBool lhs ++ formatBinOp o ++ formatBool rhs) ==
        Right (PBlnBinOpr o (PBlnLiteral lhs) (PBlnLiteral rhs))
    it "should parse unary op" $
      property $ \b ->
        parse pBlnExpression ("! " ++ formatBool b) ==
        Right (PBlnUnrOpr Not (PBlnLiteral b))
    it "sould parse PInt equality" $
      parse pBlnExpression [r| 3 == 4 |] `shouldBe`
      Right (PIntCmpOpr PIntEq (PIntLiteral 3) (PIntLiteral 4))
    it "should parse PInt expressions in comparison" $
      parse pBlnExpression [r| 1+2 != 4 |] `shouldBe`
      Right
        (PIntCmpOpr
           PIntNotEq
           (PIntBinOpr Add (PIntLiteral 1) (PIntLiteral 2))
           (PIntLiteral 4))
