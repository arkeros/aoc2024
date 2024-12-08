module Main where

import Data.Array
import Data.List (isPrefixOf, tails)

type Matrix a = Array (Int, Int) a
type Input = Matrix Char

parseInput :: String -> Input
parseInput str = listArray ((0, 0), (n - 1, m - 1)) $ concat rows
 where
  rows = lines str
  n = length rows
  m = length $ head rows

rows :: Matrix a -> [[a]]
rows matrix = [[matrix ! (i, j) | j <- [m_0 .. m]] | i <- [n_0 .. n]]
 where
  ((n_0, m_0), (n, m)) = bounds matrix

cols :: Matrix a -> [[a]]
cols matrix = [[matrix ! (i, j) | i <- [n_0 .. n]] | j <- [m_0 .. m]]
 where
  ((n_0, m_0), (n, m)) = bounds matrix

diags :: Matrix a -> [[a]]
diags matrix = map (map (matrix !)) indices
 where
  indices = [[(i, j) | i <- [n_0 .. n], j <- [m_0 .. m], i - n_0 == j - m_0 + k] | k <- [-n .. m]]
  ((n_0, m_0), (n, m)) = bounds matrix

antidiags :: Matrix a -> [[a]]
antidiags matrix = map (map (matrix !)) indices
 where
  indices = [[(i, j) | i <- [n_0 .. n], j <- [m_0 .. m], i + j == k] | k <- [0 .. n + m]]
  ((n_0, m_0), (n, m)) = bounds matrix

findPattern :: (Eq a) => [a] -> [a] -> Int
findPattern pattern = length . filter (pattern `isPrefixOf`) . tails

-- find XMAS in crosswords: check rows, columns, and diagonals. forwards and backwards.
solve1 :: Input -> Int
solve1 matrix = sum $ map (findPattern "XMAS") (xs ++ map reverse xs)
 where
  xs = rows matrix ++ cols matrix ++ diags matrix ++ antidiags matrix

solve2 :: Input -> Int
solve2 = length . filter p . submatrices3
 where
  p :: Matrix Char -> Bool
  p m = (diag == test || reverse diag == test) && (antidiag == test || reverse antidiag == test)
   where
    diag = [m ! (i, i) | i <- [-1 .. 1]]
    antidiag = [m ! (i, -i) | i <- [-1 .. 1]]
    test = "MAS"

-- Returns all 3x3 submatrices of a matrix
submatrices3 :: Matrix a -> [Matrix a]
submatrices3 matrix = [submatrix3 (i, j) | i <- [1 .. n - 1], j <- [1 .. m - 1]]
 where
  submatrix3 (x, y) = array ((-1, -1), (1, 1)) [((i, j), matrix ! (x + i, y + j)) | i <- [-1 .. 1], j <- [-1 .. 1]]
  (_, (n, m)) = bounds matrix

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print input
  print $ solve1 input
  print $ solve2 input
