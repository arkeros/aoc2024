module Main where

import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Map qualified as M
import Data.Monoid (Sum (..))

type Counter a = M.Map a (Sum Int)
type Input = [(Int, Int)]

-- in each line, there are two integers separated by some spaces
parseInput :: String -> Input
parseInput = map ((\[a, b] -> (read a, read b)) . words) . lines

distance :: (Int, Int) -> Int
distance (a, b) = abs (a - b)

sortEach :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortEach = uncurry zip . bimap sort sort . unzip

solve1 :: Input -> Int
solve1 = sum . map distance . sortEach

count :: (Ord a) => [a] -> Counter a
count = M.fromListWith (<>) . map (,Sum 1)

getCount :: (Ord a) => Counter a -> a -> Int
getCount counter x = getSum $ M.findWithDefault (Sum 0) x counter

solve2 :: Input -> Int
solve2 input = sum [similarity a | a <- as]
  where
    (as, bs) = unzip input
    counter :: Counter Int = count bs
    similarity :: Int -> Int
    similarity x = x * getCount counter x

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print $ solve1 input
    print $ solve2 input
