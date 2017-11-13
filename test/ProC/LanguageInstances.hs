module ProC.LanguageInstances
  (
  ) where

import           ProC.Language

import           Test.QuickCheck

instance Arbitrary PBlnBinOpr where
  arbitrary = elements [And, Or]

instance Arbitrary PIntBinOpr where
  arbitrary = elements [Add, Subtract, Multiply, Divide]

instance Arbitrary PStrBinOpr where
  arbitrary = elements [Concat]
