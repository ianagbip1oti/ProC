module ProC.Parser.PIntExpressionSpec
  ( spec
  ) where

import           ProC.Expectations
import           ProC.Parser.ProC
import           ProC.Parser.Statement

import           Data.Either

import           Test.Hspec

spec :: Spec
spec =
  describe "term" $ do
    it "should parse int variables" $
      parse statements "int a=1; int b=a*1+a;" `shouldSatisfyIO` isRight
    it "shold fail with str variable" $
      parse statements "str a=\"abc\"; int b=a*1+a;" `shouldSatisfyIO` isLeft
