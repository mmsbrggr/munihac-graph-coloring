module Main where

import Data.Matrix
import Parser

headlineFile = "headline.txt"
filesFolder  = "files"

main :: IO ()
main = do 
    greeting
    filename <- askForFileName
    graph <- parseFile filename
    putStrLn (prettyMatrix graph)
    pure ()

greeting :: IO ()
greeting = do 
    headline <- readFile headlineFile
    putStrLn headline
    putStrLn "Heuristic Graph Coloring"
    putStrLn "by Cornelius, Giovanni, Marcel - Munihac (2018)"
    putStrLn ""

askForFileName :: IO String
askForFileName = do 
    putStrLn "Name of DIMACS graph file:"
    filename <- getLine
    putStrLn ""
    pure filename

parseFile :: String -> IO (Matrix Int)
parseFile filename = do 
    putStrLn "Parsing file ..."
    matrix <- parseVertexFile (filesFolder ++ "/" ++ filename)
    putStrLn "... file parsed!"
    pure matrix

