module ProC.ParserSpec
  ( spec
  ) where

import           ProC.Language
import           ProC.Parser

import           Test.Hspec

spec :: Spec
spec =
  describe "parseProC" $
  it "parses Hello World" $
  parseProC "print(\"Hello World\");" `shouldBe`
  Right (Seq [Print (StringLiteral "Hello World"), Noop])
