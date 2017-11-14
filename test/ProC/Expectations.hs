module ProC.Expectations
  ( shouldSatisfyIO
  ) where

import           Test.Hspec

shouldSatisfyIO :: Show a => IO a -> (a -> Bool) -> IO ()
a `shouldSatisfyIO` e = a >>= flip shouldSatisfy e
