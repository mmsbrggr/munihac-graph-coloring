module Problem where

import Types
import SimulatedAnnealing
import Utils

runHeuristic :: Graph -> IO Int
runHeuristic g =
      do
        coloring <- initialCandidate g numChroma
        run' numChroma g initTemperature coloring 0
    where
        numChroma = maxDegree g - 1

run' :: Int -> Graph -> Temp -> Coloring -> Int -> IO Int
run' allColors g t c i
  | stop t g c = pure $ numberOfColors c
  | stopTemperatureCycle i = run' allColors g (changeTemperature t) c 0
  | otherwise = do
                   perturbation <- neighbor g allColors c
                   newColoring <- selection t g c perturbation
                   run' allColors g t newColoring (i + 1)
  where
      numChroma = numberOfColors c
