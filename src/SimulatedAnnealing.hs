module SimulatedAnnealing where

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

selection :: Graph -> OldColoring -> NewColoring -> IO Coloring
selection = undefined

changeTemperature :: Temp -> Time -> Temp
changeTemperature = undefined

stopTemperatureCycle :: a -> Bool
stopTemperatureCycle = undefined

stop :: a -> Bool
stop = undefined

