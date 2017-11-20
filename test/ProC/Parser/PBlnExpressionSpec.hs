{-# LANGUAGE QuasiQuotes #-}

module ProC.Parser.PBlnExpressionSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.PBlnExpression
import           ProC.Parser.ProC

import           Test.Hspec

import           Text.Parsec                (ParseError)
import           Text.RawString.QQ

parseE :: String -> IO (Either ParseError PBlnExpression)
parseE = parse (whiteSpace >> pBlnExpression)

spec :: Spec
spec =
  describe "pBlnExpression" $ do
    it "should parse literals" $ do
      parseE [r| tru |] `shouldReturn` Right (PBlnLiteral True)
      parseE [r| fls |] `shouldReturn` Right (PBlnLiteral False)
    it "should parse binary ops" $
      parseE [r| tru && fls || tru |] `shouldReturn`
      Right
        (PBlnBinOpr
           Or
           (PBlnBinOpr And (PBlnLiteral True) (PBlnLiteral False))
           (PBlnLiteral True))
    it "should parse unary op" $
      parseE [r| !tru |] `shouldReturn`
      Right (PBlnUnrOpr Not (PBlnLiteral True))
    it "sould parse PInt equality" $
      parseE [r| 3 == 4 |] `shouldReturn`
      Right (PIntCmpOpr PIntEq (PIntLiteral 3) (PIntLiteral 4))
    it "should parse PInt expressions in comparison" $
      parseE [r| 1+2 != 4 |] `shouldReturn`
      Right
        (PIntCmpOpr
           PIntNotEq
           (PIntBinOpr Add (PIntLiteral 1) (PIntLiteral 2))
           (PIntLiteral 4))
