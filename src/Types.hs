module Types where

import Data.Matrix
import qualified Data.Vector as V

type Temp        = Double
type Time        = Int
type Graph    = Matrix Int
type Coloring = V.Vector Int
type OldColoring = Coloring
type NewColoring = Coloring
type Rnd a = (a, a) -> IO a
