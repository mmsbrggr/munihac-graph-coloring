module Parser where

import           Control.Monad          (void)
import           Data.Either            (fromRight)
import           Data.Matrix
import qualified Data.Text              as T
import qualified Data.Text.Read         as T
import           Problem
import qualified Text.Parsec            as P
import qualified Text.Parsec.Combinator as C
import qualified Text.Parsec.Error      as P
import           Text.Parsec.String     (Parser, parseFromFile)

type MatrixGenerator = (Int, Int) -> Int
type EdgeData        = [(Int, Int)]

parseVertexFile :: String -> IO Graph
parseVertexFile filename = do result <- parseFromFile vertexFileParser filename
                              case result of
                                Left perror -> error $ unlines $ map P.messageString $ P.errorMessages perror
                                Right graph -> pure graph

whitespace :: Parser ()
whitespace = P.skipMany $ P.oneOf " \t"

stripped :: Parser a -> Parser a
stripped p = p <* whitespace

lineType :: Char -> Parser ()
lineType c = P.char c *> whitespace

number' :: Parser Int
number' = read <$> P.many1 P.digit

number :: Parser Int
number = stripped number'

header :: Parser ()
header = P.skipMany (skipLineOfType 'c')
  where
    skipLineOfType c = (lineType c <* P.manyTill P.anyChar P.endOfLine) P.<|> void P.endOfLine

fileInfo :: Parser Int
fileInfo = lineType 'p' *> P.string "edge " *> number <* P.manyTill P.anyChar P.endOfLine

vertexRecord :: Parser (Int, Int)
vertexRecord = lineType 'e' *> ((,) <$> number <*> number) <* P.endOfLine

vertexFileParser :: Parser Graph
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

