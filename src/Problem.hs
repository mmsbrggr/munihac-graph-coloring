module Problem where

import Types
import SimulatedAnnealing
import Utils

runHeuristic :: Graph -> IO Int
runHeuristic g =
      do
        coloring <- initialCandidate g numChroma
        result <- run' g initTemperature coloring 0
        pure 2
    where
        numChroma = maxDegree g - 1

run' :: Graph -> Temp -> Coloring -> Int -> IO Coloring
run' g t c i
  | stop t g c = pure c
  | stopTemperatureCycle i = run' g (changeTemperature t) c 0
  | otherwise = do
                   perturbation <- neighbor g numChroma c
                   newColoring <- selection t g c perturbation
                   run' g t newColoring (i + 1)
  where
      numChroma = numberOfColors c
