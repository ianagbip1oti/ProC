{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ProC.LanguageSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.LanguageInstances ()

import           Test.Hspec
import           Test.QuickCheck

data T a =
  T

equalsAndShow ::
     forall a. (Arbitrary a, Show a, Eq a)
  => T a
  -> SpecWith ()
equalsAndShow T = do
  it "should equal itself" $ property $ \(b :: a) -> b == b
  it "should have consistent show" $ property $ \(b :: a) -> showList [b] "" == showList [b] ""

spec :: Spec
spec = do
  describe "NumericBinOp" $ equalsAndShow @NumericBinOp T
  describe "StrBinOp" $ equalsAndShow @StrBinOp T
