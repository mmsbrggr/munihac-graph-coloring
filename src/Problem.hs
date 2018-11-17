module Problem where

import           Data.List
import qualified Data.Map    as M
import           Data.Matrix
import qualified Data.Vector as V

type Graph    = Matrix Int
type Coloring = V.Vector Int

maxDegree :: Graph -> Int
maxDegree g = maximum $ multStd g vector
    where
        nodes = nrows g
        vector = fromList nodes 1 (repeat 1)

numberOfColors :: Coloring -> Int
numberOfColors coloring = length . filter (/= 0). map snd . M.toList $ colorMap
  where zeroMap = M.fromList $ zip [1.. V.length coloring] (repeat 0)
        colorMap = foldr (M.adjust (+1)) zeroMap (V.toList coloring)

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
