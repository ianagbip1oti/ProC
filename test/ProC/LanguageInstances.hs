module ProC.LanguageInstances
  (
  ) where

import           ProC.Language

import           Test.QuickCheck

instance Arbitrary NumericBinOp where
  arbitrary = elements [Add, Subtract, Multiply, Divide]

instance Arbitrary StrBinOp where
  arbitrary = elements [Concat]
