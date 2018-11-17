module Main where

import Data.Matrix
import Parser
import Types
import Problem

headlineFile = "headline.txt"
filesFolder  = "files"

main :: IO ()
main = do
    greeting
    filename <- askForFileName
    graph <- parseFile filename
    solve graph
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

parseFile :: String -> IO Graph 
parseFile filename = do 
    putStrLn "Parsing file ..."
    matrix <- parseVertexFile (filesFolder ++ "/" ++ filename)
    putStrLn "... file parsed!"
    pure matrix

