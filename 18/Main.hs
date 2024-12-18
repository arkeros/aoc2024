{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Array ((!), (//))
import Data.Containers.ListUtils (nubOrd)
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
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

type Coordinates = (Int, Int)
type WallGrid = [Coordinates]
type Input = WallGrid

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

allDirections :: [Direction]
allDirections = [North, South, East, West]

coordinatesP :: Parser Coordinates
coordinatesP = (,) <$> L.decimal <* char ',' <*> L.decimal

inputP :: Parser Input
inputP = coordinatesP `sepEndBy` newline

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
  maxX = 70
  maxY = 70

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
type Key = Coordinates

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

allShortestPaths :: [Vertex] -> (Distances, ParentsMap) -> [Tree Vertex]
allShortestPaths targets s@(distances, parents) = map (buildPathTree parents) . filter isShortestTarget $ targets
 where
  minDistance = shortestDistance targets s
  isShortestTarget :: Vertex -> Bool
  isShortestTarget = (== minDistance) . (distances !??)

-- assumes edges are undirected
removeVertex :: Vertex -> Graph -> Graph
removeVertex v g =
  g // ((v, []) : [(v', filter (/= v) (g ! v')) | v' <- disconnected])
 where
  disconnected = g ! v

-- | Successively remove vertices from a graph, returning the intermediate graphs
peelGraph :: Graph -> [Vertex] -> [Graph]
peelGraph g = drop 1 . scanl (flip removeVertex) g

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

{- | Find the first element in a list that satisfies a predicate, with binary search.
p is monotonic. If p x = True, then p y = True for all y > x.
-}
search :: (a -> Bool) -> [a] -> Maybe a
search _ [] = Nothing
search p [x] = if p x then Just x else Nothing
search p xs = search p (if p x then left else right)
 where
  (left, right) = halve xs
  x = trace ("Binary search: " <> show (length xs)) last left

solve :: Input -> (Distance Int, Maybe Coordinates)
solve wallGrid = (part1, keyFromVertex <$> part2)
 where
  (wallHead, wallTail) = splitAt 1024 wallGrid
  part1 = shortestDistance [target] (dijkstra graph costFromEdge start)
  part2 = findBottleNeck' graph (mapMaybe vertexFromKey wallTail)

  findBottleNeck :: Graph -> [Vertex] -> Maybe Vertex
  findBottleNeck _ [] = Nothing
  findBottleNeck g (v : vs) =
    let g' = removeVertex v g
     in if path g' start target
          then findBottleNeck g' vs
          else Just v

  -- \| Find the bottleneck, but with binary search
  findBottleNeck' :: Graph -> [Vertex] -> Maybe Vertex
  findBottleNeck' g0 vs = fst <$> search (\(v, g) -> not $ path g start target) xs
   where
    -- TODO: this binary search is very wasteful of memory
    xs :: [(Vertex, Graph)]
    xs = zip vs (peelGraph g0 vs)

  -- Graph construction
  emptyGrid = negateGrid (Set.fromList wallHead)
  (graph, nodeFromVertex, vertexFromKey) =
    graphFromEdges
      [let key = cell in (cell, key, children key) | cell <- Set.toList emptyGrid]
  children :: Key -> [Key]
  children p = [p' | p' <- (`move` p) <$> allDirections, p' ∈ emptyGrid]
  keyFromNode (_, key, _) = key
  keyFromVertex = keyFromNode . nodeFromVertex

  -- Dijkstra inputs
  Just start = vertexFromKey (0, 0)
  Just target = vertexFromKey (70, 70)
  costFromEdge :: Edge -> Distance Int
  costFromEdge = const 1

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      -- print x
      print $ solve x
