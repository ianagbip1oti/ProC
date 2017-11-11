{-# LANGUAGE QuasiQuotes #-}

module ProC.Parser.BlnExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.LanguageInstances    ()
import           ProC.Parser.BlnExpression
import           ProC.Parser.ProC

import           Test.Hspec
import           Test.QuickCheck

import           Text.RawString.QQ

formatBool :: Bool -> String
formatBool True  = "tru"
formatBool False = "fls"

formatBinOp :: BlnBinOp -> String
formatBinOp And = "&&"
formatBinOp Or  = "||"

spec :: Spec
spec =
  describe "PBln Expressions" $ do
    describe "term" $ do
      it "should parse literals" $
        property $ \b -> parse blnExpression (formatBool b) == Right (Literal b)
      it "should parse binary ops" $
        property $ \o lhs rhs ->
          parse
            blnExpression
            (formatBool lhs ++ formatBinOp o ++ formatBool rhs) ==
          Right (BinaryOp o (Literal lhs) (Literal rhs))
      it "should parse unary op" $
        property $ \b ->
          parse blnExpression ("! " ++ formatBool b) ==
          Right (UnaryOp Not (Literal b))
    describe "checkExpression" $ do
      it "should parse literals" $
        property $ \b ->
          parse checkExpression (formatBool b) == Right (CheckE (Literal b))
      it "should parse int comparisons" $ do
        parse checkExpression [r| 3 == 4 |] `shouldBe`
          Right (CheckC (PIntCompare NumericEq (Literal 3) (Literal 4)))
        parse checkExpression [r| 1+2 != 4 |] `shouldBe`
          Right
            (CheckC
               (PIntCompare
                  NumericNotEq
                  (BinaryOp Add (Literal 1) (Literal 2))
                  (Literal 4)))
