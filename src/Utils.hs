module Utils where

import Data.List
import Data.Matrix
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Set    as S

import Types

nodeDegree :: Graph -> Int -> Int
nodeDegree g i = V.sum $ getRow i g

-- https://www.sciencedirect.com/science/article/pii/S0021980069800104
maxBound' :: Graph -> Int
maxBound' g = maximum $ zipWith min (map (+1) $ orderedNodesDegrees g) [1..]

orderedNodesDegrees :: Graph -> [Int]
orderedNodesDegrees g = sortOn Down $ map (nodeDegree g) [1.. nrows g]

minBound' :: Graph -> Int -> Int
minBound' g j | rhoSum bigN (orderedNodesDegrees g) j < bigN = minBound' g (j + 1)
              | otherwise                   = j
   where
       bigN = nrows g

rho orderedDegrees bigN j | j == 1 = bigN - head orderedDegrees
                          | j > 1  = bigN - (orderedDegrees !! (rhoSum bigN orderedDegrees (j - 1) + 1))


rhoSum bigN orderedDegrees 1 = rho orderedDegrees bigN 1
rhoSum bigN orderedDegrees j = rho orderedDegrees bigN j + rhoSum bigN orderedDegrees (j - 1)

-- this could work but needs more tests, then you don't need rho function
-- rhoSum bigN orderedDegrees j = j + bigN - (orderedDegrees !! (rhoSum bigN orderedDegrees (j - 1) + 1))

--memoizedRhoSum :: Graph -> [Int] -> Int -> Int
--memoizedRhoSum g orderedDegrees = (map rho [0.. ] !!)
--            where
--                bigN = nrows g
--                rho 0 = 0
--                rho n = bigN - (orderedDegrees !! (memoizedRhoSum g orderedDegrees (n - 1) + 1))

maxDegree :: Graph -> Int
maxDegree g = maximum $ multStd g vector
    where
        nodes = nrows g
        vector = fromList nodes 1 (repeat 1)

numberOfColors :: Coloring -> Int
numberOfColors = S.size . S.fromList . V.toList

numberOfConflicts :: Graph -> Coloring -> Int
numberOfConflicts g c = foldr increaseIfZero 0 (getConflictMatrix g c)
    where
        increaseIfZero x c = if x == 0 then c + 1 else c

getConflictingNodes :: Graph -> Coloring -> V.Vector Int 
getConflictingNodes g c = foldr addIfContainsZero V.empty [1..(nrows g)]
    where 
        cm = getConflictMatrix g c
        addIfContainsZero row ns = if V.elem 0 (getRow row cm) then V.cons row ns else ns

-- | Computes the conflict matrix which contains a zero exactly on edges which
-- | connect to conflicting nodes
getConflictMatrix :: Graph -> Coloring -> Matrix Int
getConflictMatrix g c = foldr modifyRow colmatrix [1..(nrows colmatrix)]
    where 
        colmatrix      = multStd g (diagonal 0 c)
        modifyRow r m  = mapRow (\_ x -> x - (c V.! (r - 1))) r m 

getRandomElement :: Rnd Int -> V.Vector a -> IO a
getRandomElement rnd v = do
    i <- rnd (0, V.length v - 1)
    pure $ v V.! i
