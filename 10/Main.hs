module Main where

import Control.Monad (guard)
import Data.Array
import Data.Containers.ListUtils (nubOrd)
import Data.List (singleton)

type Input = Array (Int, Int) Int

parseInput :: String -> Input
parseInput str = listArray ((0, 0), (n - 1, m - 1)) $ concat rows
 where
  rows :: [[Int]] = fmap (read . singleton) <$> lines str
  n = length rows
  m = length . head $ rows

neighbors :: Input -> (Int, Int) -> [(Int, Int)]
neighbors input (i, j) = filter inBounds [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
 where
  inBounds = inRange (bounds input)

solve :: ([(Int, Int)] -> Int) -> Input -> Int
solve f input = sum (score <$> trailHeads)
 where
  maxHeight = 9
  trailHeads = do
    ix <- indices input
    guard $ input ! ix == 0
    return ix
  walk ix =
    if input ! ix == maxHeight
      then return ix
      else do
        ix' <- neighbors input ix
        guard $ input ! ix' == input ! ix + 1
        return ix'
  score :: (Int, Int) -> Int
  score = f . (!! maxHeight) . iterate (>>= walk) . singleton

solve1 :: Input -> Int
solve1 = solve (length . nubOrd)

solve2 :: Input -> Int
solve2 = solve length

main :: IO ()
main = do
  input <- parseInput <$> getContents
  -- print input
  print $ solve1 input
  print $ solve2 input
