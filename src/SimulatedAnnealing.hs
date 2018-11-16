module SimulatedAnnealing where

import Data.Vector
import Problem

type OldColoring = Coloring
type NewColoring = Coloring
type Temp        = Float
type Time        = Int

initialCandidate :: Graph -> Int -> IO Coloring
initialCandidate g numberOfColors = do
    colors   <- getStdGen >>= randomRs (1, numberOfColors)
    coloring <- fromList $ take (nrows g) colors
    pure coloring

neighbor :: Graph -> Int -> Coloring -> IO Coloring
neighbor g numberOfColors = do
    gen <- getStdGen
    v <- randomR (0, (nrows g) - 1) gen
    c <- randomR (1, numberOfColors) gen 

initTemperature :: Temp 
initTemperature = undefined

selection :: Graph -> OldColoring -> NewColoring -> IO Coloring
selection = undefined

changeTemperature :: Temp -> Time -> Temp
changeTemperature = undefined

stopTemperatureCycle :: a -> Bool
stopTemperatureCycle = undefined

stop :: a -> Bool
stop = undefined

