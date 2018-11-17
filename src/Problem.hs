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

-- | Computes the number of conflicts for a graph and a given coloring:
-- | It multiplies the adjecency matrix with the coloring diagonal-matrix.
-- | Then in row i you have all the adjecent vertices in their respective color.
-- | When subtracting the color vector from each column, the number of conflicts in the
-- | original graph equals the number of zeros. That's what is computed by the function.
numberOfConflicts :: Graph -> Coloring -> Int
numberOfConflicts g c = foldr (\x c -> if x == 0 then c + 1 else c) 0 conflictmatrix
    where colmatrix      = multStd g (diagonal 0 c)
          conflictmatrix = foldr modifyRow colmatrix [1..(nrows colmatrix)]
          modifyRow r m  = mapRow (\_ x -> x - (c V.! (r - 1))) r m

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
