module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.List (find, (\\))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE

type Coordinates = (Int, Int)
type Input = UArray Coordinates Char

-- Remove from the list the first element that satisfies the predicate
removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst a (x : xs)
  | x == a = xs
  | otherwise = x : removeFirst a xs

neighbors :: Coordinates -> [Coordinates]
neighbors (!i, !j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

parseInput :: String -> Input
parseInput str = listArray ((0, 0), (n - 1, m - 1)) . concat $ rows
 where
  rows :: [[Char]] = lines str
  n = length rows
  m = length . head $ rows

type ConnectedComponent = NonEmpty Coordinates

-- solve :: ([Index] -> Int) -> Input -> Int
-- solve :: Input -> (Sum Int, Sum Int)
-- solve :: Input -> Int
solve = sum . map price . (>>= connectedComponents) . map (fmap fst) . NE.groupAllWith snd . assocs
 where
  -- price :: ConnectedComponent -> Int
  price = (*) <$> area <*> perimeter
  area :: ConnectedComponent -> Int
  area = length
  -- The perimeter is the number of neighbors that are not in the connected component
  perimeter :: ConnectedComponent -> Int
  perimeter xs = length (filter p . (>>= neighbors) $ NE.toList xs)
   where
    p :: Coordinates -> Bool
    p = not . (`elem` NE.toList xs)

distance :: Coordinates -> Coordinates -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distance' :: NonEmpty Coordinates -> Coordinates -> Int
distance' ps q = foldr f maxBound ps
 where
  -- take advantage of the fact that the minimum distance will be 1
  f _ 1 = 1
  f p acc = min acc (distance p q)

-- Split the input into connected components, ie. groups of coordinates that are adjacent to each other
connectedComponents :: NonEmpty Coordinates -> [ConnectedComponent]
connectedComponents = connectedComponents' . NE.toList
 where
  connectedComponents' :: [Coordinates] -> [ConnectedComponent]
  connectedComponents' [] = []
  connectedComponents' xs = let (component, rest) = walk (NE.fromList xs) in component : connectedComponents' rest

  -- do BFS to find the connected component
  walk :: NonEmpty Coordinates -> (ConnectedComponent, [Coordinates])
  walk (x :| xs) = let component = flip execState (NE.singleton x) $ bfs (x, xs) in (NE.nub component, xs \\ NE.toList component)

  bfs :: (Coordinates, [Coordinates]) -> State ConnectedComponent ()
  bfs (x, rest) = do
    modify (x <|)
    forM_ (neighbors x) $ \y -> do
      visited <- get
      let notVisited = y `notElem` NE.toList visited
      when (y `elem` rest && notVisited) $ bfs (y, removeFirst y rest)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  -- print input
  print $ solve input

-- print $ solve input
