module Visualisation where

import           Control.Monad
import           Data.Foldable
import           Data.Matrix
import qualified Data.Vector    as V
import           Graphics.Gloss
import           System.Random

import           Types
import           Utils

type Positioning = V.Vector (Int, Int)

width, height, offset :: Int
width = 500
height = 500
offset = 100

window :: Display
window = InWindow "Graph" (width, height) (offset, offset)

background :: Color
background = white

dot :: Picture
dot = circleSolid 10

-- TODO: Build a more sophisticated version of this
colorNode :: Int -> Int -> Color
colorNode gColor cNum = makeColor
  (fromIntegral gColor / fromIntegral cNum) 0 0 1

-- TODO: Make this monster more readable
coloringToPic :: Graph -> Coloring -> Positioning -> Picture
coloringToPic graph c pos = pictures $ edgePic : nodePic
  where gSize = length pos
        cNum = numberOfColors c
        moveNode x y = translate (toRange x) (toRange y)
        toRange x = fromIntegral $ mod x height

        nodePic :: [Picture]
        nodePic = V.toList $ fmap (\((px, py), gColor) -> moveNode px py
          . color (colorNode gColor cNum) $ dot) (V.zip pos c)

        mRows :: V.Vector (V.Vector Int)
        mRows = flip getRow graph <$> V.fromList [1..gSize]

        adjIndices :: V.Vector Int -> Int -> [Int]
        adjIndices vec nNum = snd $ foldl' (\(n, xs) adj
          -> (n+1, if adj == 1 then n:xs else xs))
          (nNum + 1, []) (drop (nNum + 1) . V.toList $ vec)

        edgesForNode :: Int -> [Int] -> [[(Int, Int)]]
        edgesForNode node = map (\x -> map (pos V.!) [node, x])

        edges :: [[(Int, Int)]]
        edges = concatMap (uncurry edgesForNode) . zip [0..]
          . map (uncurry adjIndices) $ zip (V.toList mRows) [0..]

        edgePic :: Picture
        edgePic = pictures . map line $ flxy edges
          where flxy = map $ map (\(x, y )-> (toRange x, toRange y))

positionNodes :: Int -> IO Positioning
positionNodes numNodes = liftM2 V.zip
  (V.replicateM numNodes (randomIO :: IO Int))
  (V.replicateM numNodes (randomIO :: IO Int))

visualize :: Picture -> IO ()
visualize = display window background
