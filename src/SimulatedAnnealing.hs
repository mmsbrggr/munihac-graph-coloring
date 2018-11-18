module SimulatedAnnealing where

import qualified Data.Vector as V
import           Data.Matrix
import           Data.List
import           System.Random
import           Control.Monad (replicateM)
import           Control.Monad.State.Lazy

import           Types
import           Utils

setInitialColoring :: Rnd Int -> SAState () 
setInitialColoring rnd = do
    ps <- get
    colors <- liftIO $ replicateM (nrows $ graph ps) $ rnd (1, colors ps)
    put (ps {currentColoring = V.fromList colors})

neighbor :: Rnd Int -> SAState Coloring
neighbor rnd = do
    ps <- get
    let conflictingNodes = getConflictingNodes (graph ps) (currentColoring ps)
    n <- liftIO $ getRandomElement rnd conflictingNodes
    c <- liftIO $ rnd (1, colors ps)
    pure $ (currentColoring ps) V.// [(n-1, c)]

setInitialTemperature :: SAState ()
setInitialTemperature = get >>= \ps -> put (ps {currentTemp = 10.0})

setNewColoring :: Coloring -> IO Double -> SAState () 
setNewColoring newcoloring rnd = do
    ps <- get
    let oldScore = numberOfConflicts (graph ps) (currentColoring ps)
    let newScore = numberOfConflicts (graph ps) newcoloring
    if newScore > oldScore
    then put (ps {currentColoring = newcoloring})  
    else do
        random <- liftIO $ rnd
        let boltz = boltzmann newScore oldScore (currentTemp ps)
        if random < boltz then put (ps {currentColoring = newcoloring}) else pure ()

boltzmann :: Int -> Int -> Temp -> Double 
boltzmann newScore oldScore temp = exp $ fromIntegral (newScore - oldScore) / temp

changeTemperature :: SAState ()
changeTemperature = get >>= \ps -> put (ps {currentTemp = (currentTemp ps) * 0.98})

resetTime :: SAState ()
resetTime = get >>= \ps -> put (ps {currentTime = 0})

increaseTime :: SAState ()
increaseTime = get >>= \ps -> put (ps {currentTime = currentTime ps + 1})

stopTemperatureCycle :: ProblemState -> Bool
stopTemperatureCycle = (> 100) . currentTime

stop :: ProblemState -> Bool
stop ps
  | numberOfConflicts (graph ps) (currentColoring ps) == 0 = True
  | otherwise = currentTemp ps < 0.001
    
