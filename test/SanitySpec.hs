module SanitySpec where

import           Test.Hspec

spec :: Spec
spec = do
  describe "sanity" $ do it "should be sane" $ do True `shouldBe` True
