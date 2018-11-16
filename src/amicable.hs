module Main where

import           Control.Monad.ST
import           Data.Function       (on)
import qualified Data.Set            as S
import           Data.Tuple          (swap)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

type Amicable = (Int, Int)

limit = 100000

makeDVec :: Int -> ST s (VM.MVector s Int)
makeDVec n = do
    v <- VM.replicate (n+1) 1
    mapM_ (uncurry (VM.modify v)) ([2..(n `div` 2)] >>= getArguments)
    return v
  where
    getArguments i = zip (repeat (+i)) [i*2,i*3..n]

{-makeDVec :: Int -> ST s (VM.MVector s Int)
makeDVec n = do
    v <- VM.replicate (n+1) 1
    mapM_ (makeIncrements v) [2..n]
    return v
  where
    makeIncrements v i = do
      let incrementByI = VM.modify v (+ i)
      mapM_ incrementByI [i*2,i*3..n]
-}

makeDs :: Int -> [Amicable]
makeDs n = zip [0..] ds
  where
    ds = V.toList $ V.create $ makeDVec n

findAmicableUpTo :: Int -> [Amicable]
findAmicableUpTo n = (filter onlyUnique . S.toList) $ intersectLists ds (flipped ds)
  where
    ds = makeDs n
    flipped = fmap swap
    intersectLists = S.intersection `on` S.fromList
    onlyUnique = (<) <$> fst <*> snd

main :: IO ()
main = do
  putStrLn "The sum of all amicable numbers up to 10000 is"
  let amicableNumbers = findAmicableUpTo limit >>= \(a, b) -> [a, b]
  print $ sum amicableNumbers
