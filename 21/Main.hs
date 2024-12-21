{-# LANGUAGE DerivingVia #-}

module Main where

import Control.Lens hiding (children)

import Control.Monad.Trans.State
import Data.Array.Unboxed
import Data.Graph
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (inits, isPrefixOf, permutations)
import Data.Maybe (mapMaybe)
import Data.MemoTrie (memoFix)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (unfoldTree)

type Coordinates = (Int, Int)
type Input = [String]

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

-- parseInput :: String -> Input
-- parseInput str =
--   ( Set.fromList . map fst . filter ((== '#') . snd) . assocs $ arr
--   , fst . head . filter ((== 'S') . snd) . assocs $ arr
--   , fst . head . filter ((== 'E') . snd) . assocs $ arr
--   )
--  where
--   arr :: UArray Coordinates Char = listArray ((0, 0), (n - 1, m - 1)) . concat $ rows
--   rows :: [[Char]] = lines str
--   n = length rows
--   m = length . head $ rows

move :: Direction -> Coordinates -> Coordinates
move North (x, y) = (x - 1, y)
move South (x, y) = (x + 1, y)
move East (x, y) = (x, y + 1)
move West (x, y) = (x, y - 1)

dirFromVector :: Coordinates -> Direction
dirFromVector (-1, 0) = North
dirFromVector (1, 0) = South
dirFromVector (0, 1) = East
dirFromVector (0, -1) = West
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

shortestDistance :: (Vertex -> Bool) -> (Distances, ParentsMap) -> Distance Int
shortestDistance f (distances, _) = foldr min Infinity $ ds
 where
  ds = mapMaybe (\(v, d) -> if f v then Just d else Nothing) . IntMap.toList $ distances

buildPathTree :: ParentsMap -> Vertex -> Tree Vertex
buildPathTree parents = unfoldTree (\v -> (v, concat $ parents IntMap.!? v))

-- allShortestPaths :: (Vertex -> Bool) -> (Distances, ParentsMap) -> [Tree Vertex]
-- allShortestPaths f s@(distances, parents) = map (buildPathTree parents) . filter isShortestTarget $ targets
--  where
--   minDistance = shortestDistance f s
--   isShortestTarget :: Vertex -> Bool
--   isShortestTarget = (== minDistance) . (distances !??)

type NumericKeypad = Char

toNumericButton :: Coordinates -> Maybe NumericKeypad
toNumericButton (0, 0) = Just '7'
toNumericButton (0, 1) = Just '8'
toNumericButton (0, 2) = Just '9'
toNumericButton (1, 0) = Just '4'
toNumericButton (1, 1) = Just '5'
toNumericButton (1, 2) = Just '6'
toNumericButton (2, 0) = Just '1'
toNumericButton (2, 1) = Just '2'
toNumericButton (2, 2) = Just '3'
toNumericButton (3, 1) = Just '0'
toNumericButton (3, 2) = Just 'A'
toNumericButton _ = Nothing

fromNumericButton :: NumericKeypad -> Coordinates
fromNumericButton '7' = (0, 0)
fromNumericButton '8' = (0, 1)
fromNumericButton '9' = (0, 2)
fromNumericButton '4' = (1, 0)
fromNumericButton '5' = (1, 1)
fromNumericButton '6' = (1, 2)
fromNumericButton '1' = (2, 0)
fromNumericButton '2' = (2, 1)
fromNumericButton '3' = (2, 2)
fromNumericButton '0' = (3, 1)
fromNumericButton 'A' = (3, 2)
fromNumericButton _ = error "fromNumericButton: invalid button"

allNumericButtons :: [NumericKeypad]
allNumericButtons = "1234567890A"

data DirectionalKeypad = DirectionalButton Direction | ActivateButton
  deriving (Eq, Show, Ord)

toDirectionalKeypad :: Coordinates -> Maybe DirectionalKeypad
toDirectionalKeypad (0, 1) = Just (DirectionalButton North)
toDirectionalKeypad (0, 2) = Just ActivateButton
toDirectionalKeypad (1, 0) = Just (DirectionalButton West)
toDirectionalKeypad (1, 1) = Just (DirectionalButton South)
toDirectionalKeypad (1, 2) = Just (DirectionalButton East)
toDirectionalKeypad _ = Nothing

fromDirectionalKeypad :: DirectionalKeypad -> Coordinates
fromDirectionalKeypad (DirectionalButton North) = (0, 1)
fromDirectionalKeypad ActivateButton = (0, 2)
fromDirectionalKeypad (DirectionalButton West) = (1, 0)
fromDirectionalKeypad (DirectionalButton South) = (1, 1)
fromDirectionalKeypad (DirectionalButton East) = (1, 2)

allDirectionalButtons :: [DirectionalKeypad]
allDirectionalButtons = ActivateButton : (DirectionalButton <$> allDirections)

type SearchState = Maybe (ChainState, [NumericKeypad])
type Key = SearchState

{- | Moves the robot accross the NumericKeypad, and press the button when activated.
 Returns the button pressed (if any).
-}
moveNumericRobot :: DirectionalKeypad -> StateT NumericKeypad Maybe (Maybe NumericKeypad)
moveNumericRobot (DirectionalButton dir) = do
  button <- get
  let p = fromNumericButton button
  let p' = move dir p
  case toNumericButton p' of
    Just button' -> put button'
    Nothing -> fail "Panicked"
  pure Nothing
moveNumericRobot ActivateButton = do
  button <- get
  pure (Just button)

{- | Moves the robot accross the DirectionalKeypad, and press the button when activated.
Returns the button pressed (if any).
-}
moveDirectionalRobot :: DirectionalKeypad -> StateT DirectionalKeypad Maybe (Maybe DirectionalKeypad)
moveDirectionalRobot (DirectionalButton dir) = do
  button <- get
  let p = fromDirectionalKeypad button
  let p' = move dir p
  case toDirectionalKeypad p' of
    Just button' -> put button'
    Nothing -> fail "Panicked"
  pure Nothing
moveDirectionalRobot ActivateButton = do
  button <- get
  pure (Just button)

-- We have a chain of n robots. A directional robot controls the movement of the next, and the last robot controls the numeric keypad.
type ChainState = ([DirectionalKeypad], NumericKeypad)

-- First write wins
instance Semigroup DirectionalKeypad where
  a <> b = a

-- focuses on the tail of the chain
tailChain :: Lens' ChainState ChainState
tailChain = lens getter setter
 where
  getter (x : xs, y) = (xs, y)
  getter _ = error "waka: empty chain"
  setter (x0 : _, y) (xs, y') = (x0 <| xs, y')

moveChain :: DirectionalKeypad -> StateT ChainState Maybe (Maybe NumericKeypad)
moveChain buttonHuman = do
  robots <- use _1
  mButton <- zoom (_1 . _head) $ moveDirectionalRobot buttonHuman
  case robots of
    [] -> error "No chain!"
    [lastRobot] -> case mButton of
      Just button -> zoom _2 $ moveNumericRobot button
      Nothing -> pure Nothing
    _ -> case mButton of
      Just button -> zoom tailChain $ moveChain button
      Nothing -> pure Nothing

humanPush :: SearchState -> DirectionalKeypad -> SearchState
humanPush (Just (chainState, out)) button = do
  (mPressed, chainState') <- runStateT (moveChain button) chainState
  case mPressed of
    Just button' -> Just (chainState', out |> button')
    Nothing -> Just (chainState', out)
humanPush Nothing _ = Nothing

solveSeq :: String -> Distance Int
solveSeq target = shortestDistance isTarget (dijkstra graph cost start)
 where
  isTarget :: Vertex -> Bool
  isTarget = (== target) . view _1 . nodeFromVertex

  -- Graph construction
  (graph, nodeFromVertex, vertexFromKey) =
    graphFromEdges
      ( ("", Nothing, children Nothing)
          : [ let key :: Key = Just (([robot3, robot2], robot1), output) in (output, key, children key)
            | robot3 <- allDirectionalButtons
            , robot2 <- allDirectionalButtons
            , robot1 <- allNumericButtons
            , output <- inits target
            ]
      )
  children :: Key -> [Key]
  children s
    | isPrefixOfTarget s = filter isPrefixOfTarget $ humanPush s <$> allDirectionalButtons
    | otherwise = []

  isPrefixOfTarget :: Key -> Bool
  isPrefixOfTarget (Just (_, output)) = output `isPrefixOf` target
  isPrefixOfTarget Nothing = False

  -- Dijkstra inputs
  Just start = vertexFromKey (Just ((replicate 2 ActivateButton, 'A'), ""))
  cost :: Edge -> Distance Int
  cost = const 1

solve1 :: Input -> Int
solve1 = sum . map complexity
 where
  complexity :: String -> Int
  complexity = (*) <$> unDist . solveSeq <*> numericPart
  numericPart :: String -> Int
  numericPart = read . init
  unDist :: Distance Int -> Int
  unDist (Dist x) = x
  unDist Infinity = error "unDist: Infinity"

manhattan :: Coordinates -> Coordinates -> Distance Int
manhattan p q = Dist (abs vx + abs vy)
 where
  (vx, vy) = p ^-^ q

(^-^) :: Coordinates -> Coordinates -> Coordinates
(x, y) ^-^ (x', y') = (x - x', y - y')

removeNothings :: [Maybe a] -> [Maybe a]
removeNothings = filter (not . null)

-- Use dynamic programming to solve the sequence problem
solveSeqDP :: String -> Distance Int
solveSeqDP str = sum . map (\(src, dst) -> dist (src, dst, 0)) $ (zip ((fromNumericButton 'A') : plis) plis)
 where
  plis = map fromNumericButton str

  -- Distance from src to target, at layer i
  dist :: (Coordinates, Coordinates, Int) -> Distance Int
  dist = memoFix $ \f (src, target, i) -> case i of
    0 -> shortestDistance isTarget (dijkstra graph costFromEdge start)
     where
      isTarget :: Vertex -> Bool
      isTarget = (== toNumericButton target) . view _2 . nodeFromVertex
      -- Graph construction
      (graph, nodeFromVertex, vertexFromKey) =
        graphFromEdges
          ( (Nothing, Nothing, children Nothing)
              : [(Just (fromNumericButton x), Just x, children (Just x)) | x <- allNumericButtons]
          )
      keyFromVertex :: Vertex -> Maybe NumericKeypad
      keyFromVertex = view _2 . nodeFromVertex
      cost :: (Maybe Coordinates, Maybe Coordinates) -> Distance Int
      cost (Nothing, _) = error "cost: Nothing"
      cost (_, Nothing) = error "cost: Nothing"
      cost (Just p, Just q) = let dir = dirFromVector (q ^-^ p) in _
      costFromEdge :: Edge -> Distance Int
      costFromEdge (u, v) = cost (coordFromVertex u, coordFromVertex v)
      coordFromVertex :: Vertex -> Maybe Coordinates
      coordFromVertex = view _1 . nodeFromVertex
      children :: Maybe NumericKeypad -> [Maybe NumericKeypad]
      children Nothing = []
      children (Just x) = removeNothings (toNumericButton . (`move` (fromNumericButton x)) <$> allDirections)
      Just start = vertexFromKey (toNumericButton src)
    2 -> 1 + manhattan src target
    n -> minimum (d <$> paths)
     where
      v = target ^-^ src
      verticals = replicate (abs (fst v)) (dirFromVector (signum (fst v), 0))
      horizontals = replicate (abs (snd v)) (dirFromVector (0, signum (snd v)))
      paths = filter isGoodPath $ permutations (verticals <> horizontals)
      d :: [Direction] -> Distance Int
      d = sum . map (\dir -> f (move dir src, target, n + 1))
      isGoodPath :: [Direction] -> Bool
      isGoodPath path = all (/= (0, 0)) coords
       where
        coords = scanl (flip move) src path

main :: IO ()
main = do
  let input = ["869A", "170A", "319A", "349A", "489A"]
  print $ solveSeq "029A"
  print $ solveSeqDP 2 "029A"
  print $ solve1 input