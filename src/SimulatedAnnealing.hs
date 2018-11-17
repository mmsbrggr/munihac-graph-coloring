module SimulatedAnnealing where

import qualified Data.Vector as V
import           Data.Matrix
import           Data.List
import           System.Random
import           Control.Monad (replicateM)

import           Types
import           Utils

type OldColoring = Coloring
type NewColoring = Coloring

initialCandidate :: Graph -> Int -> IO Coloring
initialCandidate g numberOfColors = do
    colors <- replicateM (nrows g) $ randomRIO (1, numberOfColors)
    pure $ V.fromList colors

neighbor :: Graph -> Int -> Coloring -> IO Coloring
neighbor g numberOfColors coloring = do
    i <- randomRIO (0, nrows g - 1)
    c <- randomRIO (1, numberOfColors)
    pure $ coloring V.// [(i, c)]

initTemperature :: Temp
initTemperature = 10.0

selection :: Temp -> Graph -> OldColoring -> NewColoring -> IO Coloring
selection temp g old new =
    if newScore > oldScore then pure new else resultScore
    where
        newScore = numberOfConflicts g new
        oldScore = numberOfConflicts g old
        resultScore = do
            random <- randomIO :: IO Double
            let boltz = boltzmann newScore oldScore temp
            if random < boltz then pure new else pure old

boltzmann :: Int -> Int -> Temp -> Double 
boltzmann newScore oldScore temp = exp $ fromIntegral (newScore - oldScore) / temp

changeTemperature :: Temp -> Temp
changeTemperature = (*0.995)

stopTemperatureCycle :: Int -> Bool
stopTemperatureCycle = (> 10)

stop :: Temp -> Graph -> Coloring -> Bool
stop t g c
    | numberOfConflicts g c == 0 = True
    | otherwise                  = t < 0.01

