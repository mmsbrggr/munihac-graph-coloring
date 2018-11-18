module Problem where

import System.Random
import Control.Monad.State.Lazy
import qualified Data.Vector as V

import Types
import SimulatedAnnealing
import Utils


solve :: Graph -> IO ()
solve g = let ps = ProblemState
                     { graph            = g
                     , colors           = (maxDegree g) - 1
                     , currentTime      = 0
                     , currentTemp      = 0
                     , currentColoring  = V.empty
                     , smallestSolution = Nothing
                     }
          in do
              (finalstate, _) <- runStateT solveProblemState ps
              case smallestSolution finalstate of
                  Nothing -> putStrLn "No coloring found!"
                  Just ss -> do
                        putStrLn "Best solution data:"
                        putStrLn $ "Colors used: " ++ (show $ numberOfColors ss)
              putStrLn ""
              pure ()

solveProblemState :: SAState ProblemState 
solveProblemState = do
    setInitialColoring randomRIO
    setInitialTemperature
    resetTime
    run
    ps <- get
    if numberOfConflicts (graph ps) (currentColoring ps) == 0
       then do
           put (ps
                { smallestSolution = Just $ currentColoring ps
                , colors           = colors ps - 1
                })
           solveProblemState
       else get

run :: SAState ()
run = do
    ps <- get
    if stop ps then pure ()
    else if stopTemperatureCycle ps
        then do
            liftIO $ (do
                putStrLn "Stopping temperature cycle!"
                putStrLn $ "Current temperature: " ++ (show $ currentTemp ps) 
                putStrLn "Current coloring data:"
                putStrLn $ "Colors used: " ++ (show $ numberOfColors (currentColoring ps))
                putStrLn $ "Conflicts: " ++ (show $ numberOfConflicts (graph ps) (currentColoring ps))
                putStrLn "" 
                )
            changeTemperature
            resetTime
            run
    else do
      newcoloring <- neighbor randomRIO
      setNewColoring newcoloring randomIO
      increaseTime
      run
