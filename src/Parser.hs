module Parser where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Either (fromRight)
import Data.Matrix
import Problem
import qualified Text.Parsec            as P
import qualified Text.Parsec.Combinator as C
import           Text.Parsec.String   (parseFromFile)

type MatrixGenerator = (Int, Int) -> Int
type EdgeData        = [(Int, Int)]

parseVertexFile :: String -> IO Graph
parseVertexFile = fmap (fromRight emptyMatrix) . parseFromFile vertexFileParser 
  where
    emptyMatrix = fromList 0 0 []

whitespace = P.skipMany $ P.oneOf " \t"
stripped p = p <* whitespace

lineType c = P.char c <* whitespace

number' = read <$> P.many1 P.digit
number = stripped number'

header = P.skipMany (skipLineOfType 'c')
  where
    skipLineOfType c = (lineType c <* P.manyTill P.anyChar P.endOfLine) P.<|> P.endOfLine

fileInfo = lineType 'p' *> number <* P.manyTill P.anyChar P.endOfLine

vertexRecord = lineType 'e' *> ((,) <$> number <*> number) <* P.endOfLine

vertexFileParser = do
  header
  numberOfVertices <- fileInfo
  edgeData <- P.many1 vertexRecord
  pure $ makeGraph numberOfVertices edgeData

makeGraph :: Int -> EdgeData -> Graph
makeGraph nv edgeData = matrix nv nv (createGenerator edgeData)

createGenerator :: EdgeData -> MatrixGenerator
createGenerator = foldr folder (const 0)
    where
      folder (v,w) mg ij = if ij == (v,w) || ij == (w,v) then 1 else mg ij

