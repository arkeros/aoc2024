{-# LANGUAGE DerivingVia #-}

module Main where

import Control.Lens hiding (children)
import Data.Array.Unboxed
import Data.Graph
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Coordinates = (Int, Int)
type WallGrid = Set Coordinates
type Input = (WallGrid, Coordinates, Coordinates)

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

allDirections :: [Direction]
allDirections = [North, South, East, West]

data Distance a = Dist a | Infinity
  deriving (Show, Eq, Functor)

instance (Ord a) => Ord (Distance a) where
  (<=) :: (Ord a) => Distance a -> Distance a -> Bool
  _ <= Infinity = True
  Infinity <= Dist _ = False
  Dist x <= Dist y = x <= y

instance (Num a) => Bounded (Distance a) where
  minBound = 0
  maxBound = Infinity

instance (Num a) => Num (Distance a) where
  Dist x + Dist y = Dist (x + y)
  _ + _ = Infinity

  Dist x * Dist y = Dist (x * y)
  _ * _ = Infinity

  abs (Dist x) = Dist (abs x)
  abs Infinity = Infinity

  signum (Dist x) = Dist (signum x)
  signum Infinity = Dist 1

  fromInteger = Dist . fromInteger

  negate (Dist x) = Dist (negate x)
  negate Infinity = error "negate Infinity"

negateGrid :: Set Coordinates -> Set Coordinates
negateGrid grid = Set.fromList [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY], (x, y) ∉ grid]
 where
  maxX = maximum . map fst . Set.toList $ grid
  maxY = maximum . map snd . Set.toList $ grid

parseInput :: String -> Input
parseInput str =
  ( Set.fromList . map fst . filter ((== '#') . snd) . assocs $ arr
  , fst . head . filter ((== 'S') . snd) . assocs $ arr
  , fst . head . filter ((== 'E') . snd) . assocs $ arr
  )
 where
  arr :: UArray Coordinates Char = listArray ((0, 0), (n - 1, m - 1)) . concat $ rows
  rows :: [[Char]] = lines str
  n = length rows
  m = length . head $ rows

clockwise :: Direction -> Direction
clockwise North = East
clockwise East = South
clockwise South = West
clockwise West = North

counterclockwise :: Direction -> Direction
counterclockwise = clockwise . clockwise . clockwise

move :: Direction -> Coordinates -> Coordinates
move North (x, y) = (x - 1, y)
move South (x, y) = (x + 1, y)
move East (x, y) = (x, y + 1)
move West (x, y) = (x, y - 1)

(∈) :: (Ord a) => a -> Set a -> Bool
(∈) = Set.member

(∉) :: (Ord a) => a -> Set a -> Bool
(∉) = Set.notMember

(!??) :: (Ord k) => Map k (Distance a) -> k -> Distance a
(!??) m k = Map.findWithDefault Infinity k m

type DijkstraState = (Set Vertex, Map Vertex (Distance Int), Heap (Heap.Entry (Distance Int) Vertex))
type CostFn = Edge -> Distance Int
type Key = (Coordinates, Direction)

dijkstra :: Graph -> CostFn -> Vertex -> Set Vertex -> Map Vertex (Distance Int)
dijkstra graph cost start targetSet = go initialState
 where
  initialVisited = Set.empty
  initialDistances = Map.singleton start 0
  initialQueue = Heap.singleton (Heap.Entry 0 start)
  initialState :: DijkstraState = (initialVisited, initialDistances, initialQueue)

  go :: DijkstraState -> Map Vertex (Distance Int)
  go (visited, distances, queue) = case Heap.viewMin queue of
    Nothing -> distances
    Just (Heap.Entry d v, queue') ->
      -- early exit
      if v ∈ targetSet
        then distances
        else
          if v ∈ visited
            then go (visited, distances, queue')
            else
              -- update the visited set
              let visited' = Set.insert v visited
                  -- update the distances map
                  neighbors = graph ! v
                  unvisitedNeighbors = filter (∉ visited) neighbors
                  s' :: DijkstraState = (visited', distances, queue')
               in go $ foldr (foldNeighbor v) s' unvisitedNeighbors

  foldNeighbor :: Vertex -> Vertex -> DijkstraState -> DijkstraState
  foldNeighbor v v' s@(visited, distances, queue) =
    if alt < d
      then (visited, Map.insert v' alt distances, Heap.insert (Heap.Entry alt v') queue)
      else s
   where
    alt = distances !?? v + cost (v, v')
    d = distances !?? v'

findShortestDistance :: Graph -> CostFn -> Vertex -> Set Vertex -> Distance Int
findShortestDistance graph cost start targetSet = minimum ((distances !??) <$> Set.toList targetSet)
 where
  distances = dijkstra graph cost start targetSet

solve1 :: Input -> Distance Int
solve1 (wallGrid, startPos, endPos) = findShortestDistance graph costFromEdge start targetSet
 where
  emptyGrid = negateGrid wallGrid
  (graph, nodeFromVertex, vertexFromKey) =
    graphFromEdges
      [ let key = (cell, dir) in (cell, key, children key)
      | cell <- Set.toList emptyGrid
      , dir <- allDirections
      ]
  cost :: (Key, Key) -> Distance Int
  cost ((u, _), (v, _)) = if u == v then 1000 else 1
  costFromEdge :: Edge -> Distance Int
  costFromEdge (u, v) = cost (keyFromVertex u, keyFromVertex v)
  keyFromVertex = view _2 . nodeFromVertex
  Just start = vertexFromKey (startPos, East)
  targetSet = Set.fromList . mapMaybe (vertexFromKey . (endPos,)) $ allDirections
  children :: Key -> [Key]
  children (cell, dir) =
    (let cell' = move dir cell in [(cell', dir) | cell' ∈ emptyGrid])
      <> [(cell, clockwise dir), (cell, counterclockwise dir)]

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print input
  print $ solve1 input
