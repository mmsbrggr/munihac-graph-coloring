module Parser where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Matrix
import Problem

type MatrixGenerator = (Int, Int) -> Int
type EdgeData        = [(Int, Int)]

parse :: T.Text -> Graph
parse text = matrix nv nv generator
    where nv        = numberVertices text
          generator = createGenerator (edgeData text)

numberVertices :: T.Text -> Int
numberVertices text = convertWord line 2
    where line = head $ getLinesBeginningWith 'p' text

edgeData :: T.Text -> EdgeData
edgeData text = map convert lines 
    where lines     = getLinesBeginningWith 'e' text
          convert t = (convertWord t 1, convertWord t 2) 

convertWord :: T.Text -> Int -> Int
convertWord t i = read . T.unpack $ (T.words t) !! i

getLinesBeginningWith :: Char -> T.Text -> [T.Text]
getLinesBeginningWith c t = filter ((==) c .  T.head) $ T.lines t

createGenerator :: EdgeData -> MatrixGenerator
createGenerator = foldr folder (\_ -> 0) 
    where folder (v,w) mg = \ij -> if ij == (v,w) || ij == (w,v) then 1 else mg ij


