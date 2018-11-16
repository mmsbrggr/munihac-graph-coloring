module SimulatedAnnealing where

import qualified Data.Vector as V
import           Data.Matrix
import           Data.List
import           System.Random
import           Problem

type OldColoring = Coloring
type NewColoring = Coloring
type Temp        = Float
type Time        = Int

initialCandidate :: Graph -> Int -> IO Coloring
initialCandidate g numberOfColors = do
    gen <- getStdGen
    let colors = randomRs (1, numberOfColors)
    let coloring = V.fromList $ take (nrows g) colors
    pure coloring

neighbor :: Graph -> Int -> Coloring -> IO Coloring
neighbor g numberOfColors = do
    gen      <- getStdGen
    (v, gen) <- randomR (0, (nrows g) - 1) gen
    (c, gen) <- randomR (1, numberOfColors) gen 
    pure undefined

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

changeTemperature :: Temp -> Temp
changeTemperature = (*0.9)

stopTemperatureCycle :: Int -> Bool
stopTemperatureCycle = (> 1000)

stop :: a -> Bool
stop = undefined

