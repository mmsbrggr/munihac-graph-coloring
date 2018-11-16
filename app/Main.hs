module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Matrix
import Loader
import Parser

headlineFile = "headline.txt"

main :: IO ()
main = greeting >> askForFile >>= parseFile >> pure ()

greeting :: IO ()
greeting = do headline <- readFile headlineFile 
              putStrLn headline
              putStrLn "Heuristic Graph Coloring"
              putStrLn "by Marcel Moosbrugger (2018)"
              putStrLn ""

askForFile :: IO T.Text
askForFile = do putStrLn "Name of DIMACS graph file:"
                name <- getLine
                putStrLn ""
                putStrLn "Loading file ..."
                file <- loadFile $ name
                putStrLn "... File loaded!"
                putStrLn ""
                pure file

parseFile :: T.Text -> IO (Matrix Int)
parseFile text = do putStrLn "Parsing file ..."
                    let !matrix = parse text
                    putStrLn "... file parsed!"
                    pure matrix

