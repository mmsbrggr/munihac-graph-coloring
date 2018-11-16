module SimulatedAnnealing where

import System.Random

import Problem

type OldColoring = Coloring
type NewColoring = Coloring
type Temp        = Float
type Time        = Int

initialCandidate :: Int -> IO Coloring
initialCandidate = undefined

neighbor :: Coloring -> IO Coloring
neighbor = undefined

initTemperature :: Temp 
initTemperature = undefined

selection :: Temp -> Graph -> OldColoring -> NewColoring -> IO Coloring
selection temp g old new =
    if newScore > oldScore then pure new else resultScore
    where
        newScore :: Int
        newScore = numberOfConflicts g new

        oldScore :: Int
        oldScore = numberOfConflicts g old

        resultScore :: IO Coloring
        resultScore = do
            random <- randomIO :: IO Float
            if random < boltzmann newScore oldScore temp then pure new else pure old

boltzmann :: Int -> Int -> Temp -> Float
boltzmann newScore oldScore temp = exp $ fromIntegral (newScore - oldScore) / temp

changeTemperature :: Temp -> Time -> Temp
changeTemperature = undefined

stopTemperatureCycle :: a -> Bool
stopTemperatureCycle = undefined

stop :: a -> Bool
stop = undefined

