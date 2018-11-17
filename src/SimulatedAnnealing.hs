module SimulatedAnnealing where

import qualified Data.Vector as V
import           Data.Matrix
import           Data.List
import           System.Random

import           Types
import           Utils

type OldColoring = Coloring
type NewColoring = Coloring

initialCandidate :: Graph -> Int -> IO Coloring
initialCandidate g numberOfColors = do
    gen          <- getStdGen
    let colors   = randomRs (1, numberOfColors) gen
    let coloring = V.fromList $ take (nrows g) colors
    pure coloring

neighbor :: Graph -> Int -> Coloring -> IO Coloring
neighbor g numberOfColors coloring = do
    gen          <- getStdGen
    let (i, g1) = randomR (0, nrows g - 1) gen
    let (c, _)  = randomR (1, numberOfColors) g1
    pure $ coloring V.// [(i, c)]

initTemperature :: Temp 
initTemperature = 1000.0 

selection :: Temp -> Graph -> OldColoring -> NewColoring -> IO Coloring
selection temp g old new =
    if newScore > oldScore then pure new else resultScore
    where
        newScore = numberOfConflicts g new
        oldScore = numberOfConflicts g old
        resultScore = do
            random <- randomIO :: IO Double 
            if random < boltzmann newScore oldScore temp then pure new else pure old

boltzmann :: Int -> Int -> Temp -> Double 
boltzmann newScore oldScore temp = exp $ fromIntegral (newScore - oldScore) / temp

changeTemperature :: Temp -> Temp
changeTemperature = (*0.9)

stopTemperatureCycle :: Int -> Bool
stopTemperatureCycle = (> 1000)

stop :: Temp -> Graph -> Coloring -> Bool
stop t g c
    | numberOfConflicts g c == 0 = True
    | otherwise                  = t < 0.00000000001

