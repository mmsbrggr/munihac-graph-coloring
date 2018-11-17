module Problem where

import System.Random

import Types
import SimulatedAnnealing
import Utils

solve :: Graph -> IO Int
solve g = do
    let numcolors = (maxDegree g) - 1
    solveForNumColors g numcolors

solveForNumColors :: Graph -> Int -> IO Int
solveForNumColors g numcolors = do
    coloring <- initialCandidate g numcolors 
    run numcolors g initTemperature coloring 0

run :: Int -> Graph -> Temp -> Coloring -> Int -> IO Int
run numcolors g t c i
  | stop t g c             = pure $ numberOfColors c
  | stopTemperatureCycle i = do
      putStrLn "Stopping temperature cycle:"
      let newtemp = changeTemperature t
      putStrLn $ "New temperature: " ++ (show t)
      putStrLn $ "Conflicts in coloring: " ++ (show $ numberOfConflicts g c)
      putStrLn $ "Number of colors in coloring: " ++ (show $ numberOfColors c)
      putStrLn ""
      run numcolors g newtemp c 0
  | otherwise              = do
      perturbation <- neighbor g numcolors c randomRIO
      newColoring  <- selection t g c perturbation randomIO
      run numcolors g t newColoring (i + 1)
