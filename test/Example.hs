
module Example where

import Test.Hspec
import Test.QuickCheck

example :: Spec
example =
    describe "example" $
       it "OMG who would have thought" $
            True `shouldBe` True
