module ProblemSpec where

import           Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import           Problem
import           Types
import           Utils

toyGraph :: Graph
toyGraph = M.fromList 5 5 [ 0, 1, 0, 0, 1
                        , 1, 0, 1, 1, 0
                        , 0, 1, 0, 0, 0
                        , 0, 1, 0, 0, 1
                        , 1, 0, 0, 1, 0]

coloring1 :: Coloring
coloring1 = V.fromList [1, 2, 2, 3, 2]

coloring2 :: Coloring
coloring2 = V.fromList [1, 2, 3, 3, 2]

spec :: Spec
spec = do
    describe "numberOfColors" $
       it "should return the number of colors" $
           numberOfColors coloring1 `shouldBe` 3

    describe "numberOfConflicts" $ do
        it "should return the right amount of conflicts" $
            numberOfConflicts toyGraph coloring1 `shouldBe` 2

        it "should return no conflicts" $
            numberOfConflicts toyGraph coloring2 `shouldBe` 0 
    describe "maxDegree" $
        it "the two functions are equal" $ do
            let mm = maxDegree toyGraph
            maxDegree toyGraph `shouldBe` mm
