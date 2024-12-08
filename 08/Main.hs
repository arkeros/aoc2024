module Main where

import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Linear.V2
import Linear.Vector (Additive ((^+^), (^-^)), negated)

type Antenna = Char
type Coordinates = V2 Int
type Input = ([(Coordinates, Antenna)], Coordinates)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = ((x :) <$> combinations (k - 1) xs) <> combinations k xs

pairs :: NonEmpty a -> [(a, a)]
pairs = map (\[a, b] -> (a, b)) . combinations 2 . NE.toList

parseInput :: String -> Input
parseInput str = (M.assocs $ M.filter (/= '.') grid, V2 n m)
 where
  rows = lines str
  grid :: Map Coordinates Char
  grid = M.fromList [(V2 i j, c) | (i, row) <- zip [0 ..] rows, (j, c) <- zip [0 ..] row]
  n = length rows
  m = length $ head rows

isInside :: Input -> Coordinates -> Bool
isInside (_, V2 n m) (V2 x y) = x >= 0 && x < n && y >= 0 && y < m

solve :: ((Coordinates, Coordinates) -> [Coordinates]) -> Input -> Int
solve antinodes input@(xs, _) = countUnique . filter (isInside input) . (>>= antinodes) . (>>= pairs) $ groups
 where
  groups = map (fmap fst) $ NE.groupAllWith snd xs
  countUnique = length . nubOrd

solve1 :: Input -> Int
solve1 = solve antinodes
 where
  antinodes (p, q) = let v = q ^-^ p in [p ^-^ v, q ^+^ v]

solve2 :: Input -> Int
solve2 input = solve antinodes input
 where
  line p v = takeWhile (isInside input) (iterate (^+^ v) p)
  antinodes (p, q) = let v = q ^-^ p in line q v <> line p (negated v)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print input
  print $ solve1 input
  print $ solve2 input
