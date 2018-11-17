{-# LANGUAGE ScopedTypeVariables #-}

module SimulatedAnnealingSpec where

import           Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import qualified Data.Matrix as M
import qualified Data.Vector as V

import           SimulatedAnnealing
import           Utils
import           Types

toyGraph :: Graph
toyGraph = M.fromList 5 5 [ 0, 1, 0, 0, 1
                          , 1, 0, 1, 1, 0
                          , 0, 1, 0, 0, 0
                          , 0, 1, 0, 0, 1
                          , 1, 0, 0, 1, 0]

spec :: Spec
spec = do
    describe "Boltzmann function" $ do
        it "scores are both 0" $
            boltzmann 0 0 0.5 `shouldBe` 1

        it "if scores are equal then it always returns 1" $
            boltzmann 5 5 0.5 `shouldBe` 1
    
    describe "initialCandidate function" $ do
        it "checks length of a 5x5 always returns 5" $ do
            res <- initialCandidate toyGraph 2
            length res `shouldBe` 5

        it "checks the random generator is incremented correctly" $ do
            res1 <- initialCandidate toyGraph 2
            res2 <- initialCandidate toyGraph 2
            res1 `shouldNotBe` res2

    describe "neighbot function" $ do
        it "checks the random generator is incremented correctly" $ do
            initial <- initialCandidate toyGraph 2
            res1 <- neighbor toyGraph 2 initial
            res2 <- neighbor toyGraph 2 initial
            res1 `shouldNotBe` res2
