module Problem where

import System.Random

import Data.Matrix (nrows)
import Types
import SimulatedAnnealing
import Utils

solve :: Graph -> IO ()
solve g = solveForNumColors g (maxBound' g)

solveForNumColors :: Graph -> Int -> IO ()
solveForNumColors g numcolors = do
    coloring  <- initialCandidate g numcolors randomRIO
    candidate <- run numcolors g initTemperature coloring 0
    putStrLn "Coloring found!"
    putStrLn $ "Conflicts in coloring: " ++ (show $ numberOfConflicts g candidate)
    putStrLn $ "Number of colors in coloring: " ++ (show $ numberOfColors candidate)
    if numberOfConflicts g candidate == 0
       then mayIProceedPlease g candidate
       else pure ()

mayIProceedPlease :: Graph -> Coloring -> IO ()
mayIProceedPlease g candidate | minBound' g 1 >= numberOfColors candidate = pure ()
                              | otherwise = solveForNumColors g (numberOfColors candidate - 1)

run :: Int -> Graph -> Temp -> Coloring -> Int -> IO Coloring
run numcolors g t c i
  | stop t g c = pure c
  | stopTemperatureCycle i = do
      putStrLn "Stopping temperature cycle:"
      let newtemp = changeTemperature t
      putStrLn $ "New temperature: " ++ (show t)
      putStrLn $ "Conflicts in coloring: " ++ (show $ numberOfConflicts g c)
      putStrLn $ "Number of colors in coloring: " ++ (show $ numberOfColors c)
      putStrLn ""
      run numcolors g newtemp c 0
  | otherwise = do
      perturbation <- neighbor g numcolors c randomRIO
      newColoring  <- selection t g c perturbation randomIO
      run numcolors g t newColoring (i + 1)
