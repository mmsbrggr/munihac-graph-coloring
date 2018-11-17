module Main where

import Data.Matrix
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever, void)

import Parser
import Problem
import Types

headlineFile = "headline.txt"
filesFolder  = "files"

main :: IO ()
main = do
    greeting
    filename <- askForFileName
    graph <- parseFile filename
--  putStrLn (prettyMatrix graph)
    channel <- newChan
    forkIO $ worker channel graph
    gossip channel
    pure ()

gossip :: Chan String -> IO ()
gossip chan =
      forever $ do
        gossip <- readChan chan
        putStrLn gossip

worker :: Chan String -> Graph -> IO ()
worker channel graph = do
    chromaticN <- runHeuristic graph
    writeChan channel (show chromaticN)

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

