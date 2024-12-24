{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Graph
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (unfoldTree)
import Debug.Trace (trace)

type Coordinates = (Int, Int)
type WallGrid = [Coordinates]
type Input = (WallGrid, Coordinates, Coordinates)

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

allDirections :: [Direction]
allDirections = [North, South, East, West]

parseInput :: String -> Input
parseInput str =
  ( map fst . filter ((== '#') . snd) . assocs $ arr
  , fst . head . filter ((== 'S') . snd) . assocs $ arr
  , fst . head . filter ((== 'E') . snd) . assocs $ arr
  )
 where
  arr :: Array Coordinates Char = listArray ((0, 0), (n - 1, m - 1)) . concat $ rows
  rows :: [[Char]] = lines str
  n = length rows
  m = length . head $ rows

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

class BelongsTo t a where
  (∈) :: a -> t -> Bool
  (∉) :: a -> t -> Bool

instance (Eq a) => BelongsTo [a] a where
  (∈) = elem
  (∉) = notElem

instance (Ord a) => BelongsTo (Set a) a where
  (∈) = Set.member
  (∉) = Set.notMember

instance BelongsTo IntSet IntSet.Key where
  (∈) = IntSet.member
  (∉) = IntSet.notMember

(!??) :: IntMap (Distance a) -> IntMap.Key -> Distance a
(!??) m k = IntMap.findWithDefault Infinity k m

type Distances = IntMap (Distance Int)
type Queue = Heap (Heap.Entry (Distance Int) Vertex)
type ParentsMap = IntMap [Vertex]
type DijkstraState = (IntSet, Distances, Queue, ParentsMap)
type CostFn = Edge -> Distance Int
type Key = (Coordinates, Int)

dijkstra :: Graph -> CostFn -> Vertex -> (Distances, ParentsMap)
dijkstra graph cost start = go initialState
 where
  initialState :: DijkstraState =
    ( IntSet.empty
    , IntMap.singleton start 0
    , Heap.singleton (Heap.Entry 0 start)
    , IntMap.empty
    )

  go :: DijkstraState -> (Distances, ParentsMap)
  go (visited, distances, queue, parents) = case Heap.viewMin queue of
    Nothing -> (distances, parents)
    Just (Heap.Entry d v, queue') ->
      if v ∈ visited
        then go (visited, distances, queue', parents)
        else
          let visited' = IntSet.insert v visited
              neighbors = graph ! v
              unvisitedNeighbors = filter (∉ visited) neighbors
              s' :: DijkstraState = (visited', distances, queue', parents)
           in go $ foldr (foldNeighbor v) s' unvisitedNeighbors

  foldNeighbor :: Vertex -> Vertex -> DijkstraState -> DijkstraState
  foldNeighbor v v' s@(visited, distances, queue, parents) = case compare alt d of
    LT -> (visited, IntMap.insert v' alt distances, Heap.insert (Heap.Entry alt v') queue, IntMap.insert v' [v] parents)
    EQ -> (visited, distances, queue, IntMap.adjust (v :) v' parents)
    GT -> s
   where
    alt = distances !?? v + cost (v, v')
    d = distances !?? v'

shortestDistance :: [Vertex] -> (Distances, ParentsMap) -> Distance Int
shortestDistance targets (distances, _) = minimum ((distances !??) <$> targets)

buildPathTree :: ParentsMap -> Vertex -> Tree Vertex
buildPathTree parents = unfoldTree (\v -> (v, concat $ parents IntMap.!? v))

solve1 :: Input -> Int
solve1 (walls, src, target) = length . filter ((<= cheatlessDistance - 100) . fst) . map (\edge -> (distThrough edge, edge)) $ cheats
 where
  (distances, parents) = (dijkstra graph costFromEdge start)
  (distancesFromEnd, parentsFromEnd) = (dijkstra graph costFromEdge end)
  cheatlessDistance = shortestDistance [endCheatless] (distances, parents)
  cheatDistance = shortestDistance [end] (distances, parents)
  cheats :: [Edge] =
    (>>= (\u -> map (u,) $ graph ! u))
      . mapMaybe vertexFromKey
      $ [(cell, 1) | cell <- Set.toList wallGrid]

  distThrough :: Edge -> Distance Int
  distThrough (u, v) = distances !?? u + 1 + distancesFromEnd !?? v

  -- Graph construction
  wallGrid = Set.fromList walls
  emptyGrid = negateGrid wallGrid
  (graph, nodeFromVertex, vertexFromKey) =
    graphFromEdges $
      [let key = (cell, 2) in (cell, key, children key) | cell <- Set.toList emptyGrid]
        <> [let key = (cell, 0) in (cell, key, children key) | cell <- Set.toList emptyGrid]
        <> [let key = (cell, 1) in (cell, key, children key) | cell <- Set.toList wallGrid]
  children :: Key -> [Key]
  children (p, 0) = [(p', 0) | p' <- (`move` p) <$> allDirections, p' ∈ emptyGrid]
  children (p, 1) = children (p, 0)
  children (p, 2) =
    [(p', 2) | p' <- (`move` p) <$> allDirections, p' ∈ emptyGrid]
      <> [(p', 1) | p' <- (`move` p) <$> allDirections, p' ∈ wallGrid]
  children (p, n) = error $ "invalid key: " <> show n
  keyFromNode (_, key, _) = key
  keyFromVertex = keyFromNode . nodeFromVertex

  -- Dijkstra inputs
  Just start = vertexFromKey (src, 2)
  Just end = vertexFromKey (target, 0)
  Just endCheatless = vertexFromKey (target, 2)
  costFromEdge :: Edge -> Distance Int
  costFromEdge = const 1

main :: IO ()
main = do
  input <- parseInput <$> getContents
  -- print input
  print $ solve1 input
