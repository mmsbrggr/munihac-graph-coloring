module Types where

import qualified Data.Vector as V
import           Data.Matrix
import Control.Monad.State.Lazy

type Temp        = Double
type Time        = Int
type Graph       = Matrix Int
type Coloring    = V.Vector Int
type Rnd a = (a, a) -> IO a

type SAState a = StateT ProblemState IO a

data ProblemState = ProblemState 
    { graph            :: Graph
    , colors           :: Int
    , currentTime      :: Time
    , currentTemp      :: Temp
    , currentColoring  :: Coloring
    , smallestSolution :: Maybe Coloring
    }
