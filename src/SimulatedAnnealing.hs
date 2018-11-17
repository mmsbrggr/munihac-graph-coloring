module SimulatedAnnealing where

import qualified Data.Vector as V
import           Data.Matrix
import           Data.List
import           System.Random
import           Control.Monad (replicateM)

import           Types
import           Utils

initialCandidate :: Graph -> Int -> Rnd Int -> IO Coloring
initialCandidate g numberOfColors rnd = do
    colors <- replicateM (nrows g) $ rnd (1, numberOfColors)
    pure $ V.fromList colors

neighbor :: Graph -> Int -> Coloring -> Rnd Int -> IO Coloring
neighbor g numberOfColors coloring rnd = do
    let conflictingNodes = getConflictingNodes g coloring
    n <- getRandomElement rnd conflictingNodes
    c <- rnd (1, numberOfColors)
    pure $ coloring V.// [(n-1, c)]

initTemperature :: Temp
initTemperature = 10.0

selection :: Temp -> Graph -> OldColoring -> NewColoring -> IO Double -> IO Coloring
selection temp g old new rnd =
    if newScore > oldScore then pure new else resultScore
    where
        newScore = numberOfConflicts g new
        oldScore = numberOfConflicts g old
        resultScore = do
            random <- rnd
            let boltz = boltzmann newScore oldScore temp
            if random < boltz then pure new else pure old

boltzmann :: Int -> Int -> Temp -> Double 
boltzmann newScore oldScore temp = exp $ fromIntegral (newScore - oldScore) / temp

changeTemperature :: Temp -> Temp
changeTemperature = (* 0.98)

stopTemperatureCycle :: Int -> Bool
stopTemperatureCycle = (> 100)

stop :: Temp -> Graph -> Coloring -> Bool
stop t g c
    | numberOfConflicts g c == 0 = True
    | otherwise                  = t < 0.001

