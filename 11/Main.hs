module Main where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . words

nDigits :: Int -> Int
nDigits = length . show

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

-- 2024 -> (20, 24), 99 -> (9, 9)
splitInHalf :: Int -> (Int, Int)
splitInHalf n = divMod n (10 ^ (nDigits n `div` 2))

blink :: Int -> [Int]
blink 0 = [1]
blink n =
  if (even . nDigits) n
    then pairToList . splitInHalf $ n
    else [n * 2024]

solve :: Int -> [Int] -> Int
solve n = length . (!! n) . iterate (>>= blink)

count :: [Int] -> IntMap Int
count = foldr (\x -> IntMap.insertWith (+) x 1) IntMap.empty

-- blink, but with count on the right
blink' :: (Int, Int) -> [(Int, Int)]
blink' (x, counter) = (,counter) <$> blink x

-- solve :: Int -> Input -> Int

fromList :: [(Int, Int)] -> IntMap Int
fromList = foldr (\(x, y) -> IntMap.insertWith (+) x y) IntMap.empty

solve' :: Int -> [Int] -> Int
solve' n = sum . (!! n) . iterate (fromList . (>>= blink') . IntMap.toList) . count

solve1 :: [Int] -> Int
solve1 = solve' 25

solve2 :: [Int] -> Int
solve2 = solve' 75

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ solve1 input
  print $ solve2 input
