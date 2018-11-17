module Visualisation where

import           Data.Matrix
import qualified Data.Vector    as V
import           Graphics.Gloss

import           Types
import           Utils

type Positioning = V.Vector (Int, Int)

width, height, offset :: Int
width = 520
height = 520
offset = 100

window :: Display
window = InWindow "Graph" (width, height) (offset, offset)

background :: Color
background = white

dot :: Picture
dot = circleSolid 5

-- TODO: Build a more sophisticated version of this
colorNode :: Int -> Int -> Color
colorNode gColor cNum = makeColor
  (fromIntegral gColor / fromIntegral cNum) 0 0 1

-- TODO: Make this monster more readable
-- TODO: Build edges
coloringToPic :: Graph -> Coloring -> Positioning -> Picture
coloringToPic graph c pos = pictures $ V.toList $ fmap (\((px, py), gColor)
  -> moveNode px py . color (colorNode gColor cNum) $ dot)  (V.zip pos c)
  where gSize = length pos
        cNum = numberOfColors c
        moveNode x y = translate (fromIntegral x) (fromIntegral y)

-- TODO: Implement, maybe use randomness
positionNodes :: Int -> IO Positioning
positionNodes numNodes = undefined

-- some Example
drawing :: Picture
drawing = pictures
  [ translate (-20) (-100) $ color ballColor $ circleSolid 30
  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)

visualize :: IO ()
visualize = display window background drawing
