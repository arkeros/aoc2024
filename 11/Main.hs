module Main where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . words

nDigits :: Int -> Int
nDigits n = floor $ log10 (fromIntegral n) + 1
 where
  log10 :: Float -> Float
  log10 = logBase 10

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

-- 2024 -> (20, 24), 99 -> (9, 9)
splitInHalf :: Int -> (Int, Int)
splitInHalf n = n `divMod` (10 ^ (nDigits n `div` 2))

blink :: Int -> [Int]
blink 0 = [1]
blink n =
  if (even . nDigits) n
    then pairToList . splitInHalf $ n
    else [n * 2024]

solve :: Int -> [Int] -> Int
solve n = length . (!! n) . iterate (>>= blink)

type Counter = IntMap Int

count :: [Int] -> Counter
count = foldr (\x -> IntMap.insertWith (+) x 1) IntMap.empty

-- blink, but with count on the right
blink' :: (Int, Int) -> [(Int, Int)]
blink' (x, counter) = (,counter) <$> blink x

solve' :: Int -> [Int] -> Int
solve' n = sum . (!! n) . iterate (fromList . (>>= blink') . toList) . count
 where
  fromList = IntMap.fromListWith (+)
  toList = IntMap.toList

solve1 :: [Int] -> Int
solve1 = solve' 25

solve2 :: [Int] -> Int
solve2 = solve' 75

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ solve1 input
  print $ solve2 input
