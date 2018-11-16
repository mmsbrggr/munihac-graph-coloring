module Problem where

import Data.Matrix
import qualified Data.Vector as V

type Graph    = Matrix Int
type Coloring = V.Vector Int

numberOfColors :: Coloring -> Int
numberOfColors = V.maximum

numberOfConflicts :: Graph -> Coloring -> Int
numberOfConflicts g c = foldr (\x c -> if x == 0 then c + 1 else c) 0 conflictmatrix
    where colmatrix      = multStd g (diagonal 0 c)
          conflictmatrix = foldr modifyRow colmatrix [1..(nrows colmatrix)]
          modifyRow r m  = mapRow (\_ x -> x - (c V.! (r - 1))) r m 
