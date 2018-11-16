module SimulatedAnnealingSpec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import SimulatedAnnealing

spec :: Spec
spec =
    describe "Boltzmann function" $ do
        it "scores are both 0" $
            boltzmann 0 0 0.5 `shouldBe` 1
        it "if scores are equal then it always returns 1" $
            boltzmann 5 5 0.5 `shouldBe` 1
