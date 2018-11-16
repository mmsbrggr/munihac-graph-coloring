module Loader where

import qualified Data.Text as T
import qualified Data.Text.IO as T

type FileName = String

loadFile :: FileName -> IO T.Text 
loadFile = T.readFile . ("files/" ++)

