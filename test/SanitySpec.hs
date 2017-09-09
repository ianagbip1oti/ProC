module SanitySpec
  ( spec
  ) where

import           Test.Hspec

spec :: Spec
spec = describe "sanity" $ it "should be sane" $ True `shouldBe` True
