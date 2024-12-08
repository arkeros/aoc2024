module Main where

type Level = Int
type Report = [Level]
type Input = [Report]

-- in each line, there are two integers separated by some spaces
parseInput :: String -> Input
parseInput = map parseReport . lines
 where
  parseReport :: String -> Report
  parseReport = map read . words

sameSign :: [Int] -> Bool
sameSign xs = all (== 1) (signum <$> xs) || all (== -1) (signum <$> xs)

-- Return True if all diffs have the same sign (using signum) and less than 3 in absolute value
isSafe :: Report -> Bool
isSafe xs = sameSign diffs && all (<= 3) (abs <$> diffs)
 where
  diffs :: [Int] = zipWith (-) xs (tail xs)

-- Return the number of safe reports
solve1 :: Input -> Int
solve1 = length . filter isSafe

{- | The 'oneRemoved' function takes a list and returns a list of lists,
| where each sublist is the original list with one element removed.
|
| For example:
|
| >>> oneRemoved [1, 2, 3]
| [[2, 3], [1, 3], [1, 2]]
|
| The function works by recursively removing the first element of the list
| and then prepending it to the result of recursively calling 'oneRemoved'
| on the rest of the list.
-}
oneRemoved :: [a] -> [[a]]
oneRemoved [] = []
oneRemoved (x : xs) = xs : map (x :) (oneRemoved xs)

solve2 :: Input -> Int
solve2 = length . filter isSafe'
 where
  isSafe' :: Report -> Bool
  isSafe' = any isSafe . (\xs -> [xs] <> oneRemoved xs)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ solve1 input
  print $ solve2 input
