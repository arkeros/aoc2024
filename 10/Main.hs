module Main where

import Control.Monad (guard)
import Data.Array.Unboxed
import Data.Char (digitToInt)
import Data.Containers.ListUtils (nubOrd)
import Data.List (singleton)

type Index = (Int, Int)
type Input = UArray Index Int

parseInput :: String -> Input
parseInput str = listArray ((0, 0), (n - 1, m - 1)) . concat $ rows
 where
  rows :: [[Int]] = fmap digitToInt <$> lines str
  n = length rows
  m = length . head $ rows

solve :: ([Index] -> Int) -> Input -> Int
solve f input = sum (score <$> trailHeads)
 where
  maxHeight = 9
  trailHeads :: [Index]
  trailHeads = [ix | (ix, val) <- assocs input, val == 0]
  walk :: Index -> [Index]
  walk !ix = do
    ix' <- neighbors ix
    guard $ input ! ix' == input ! ix + 1
    pure ix'
  neighbors :: Index -> [Index]
  neighbors (!i, !j) = filter inBounds [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
  inBounds :: Index -> Bool
  inBounds = inRange (bounds input)
  score :: Index -> Int
  score = f . (!! maxHeight) . iterate (>>= walk) . singleton

solve1 :: Input -> Int
solve1 = solve countUnique
 where
  countUnique = length . nubOrd

solve2 :: Input -> Int
solve2 = solve length

main :: IO ()
main = do
  input <- parseInput <$> getContents
  -- print input
  print $ solve1 input
  print $ solve2 input
