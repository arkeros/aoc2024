{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad

import Control.Monad.State.Strict
import Control.Monad.Trans.Reader
import Data.Containers.ListUtils (nubOrd)
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

data Direction = North | East | South | West
  deriving (Show, Eq)

type Coordinate = (Int, Int)
type BoundingBox = (Coordinate, Coordinate)
type Input = (Set Coordinate, Coordinate)

-- type GridM = RWS (Set Coordinate) [Coordinate] (Coordinate, Direction)
type GridM = ReaderT (Set Coordinate) (State [(Coordinate, Direction)])

keysWhere :: (a -> Bool) -> Map k a -> [k]
keysWhere p = M.keys . M.filter p

parseInput :: String -> Input
parseInput str =
  ( S.fromList . keysWhere (== '#') $ matrix
  , head . keysWhere (== '^') $ matrix
  )
 where
  matrix :: Map (Int, Int) Char
  matrix =
    M.fromList
      [ ((i, j), c)
      | (i, line) <- zip [0 ..] (lines str)
      , (j, c) <- zip [0 ..] line
      ]

walk :: Direction -> Coordinate -> Coordinate
walk North (x, y) = (x - 1, y)
walk East (x, y) = (x, y + 1)
walk South (x, y) = (x + 1, y)
walk West (x, y) = (x, y - 1)

rotate :: Direction -> Direction
rotate North = East
rotate East = South
rotate South = West
rotate West = North

calcSize :: Set Coordinate -> (Int, Int)
calcSize = S.foldl' (\(w, h) (x, y) -> (max w x, max h y)) (0, 0)

isInside :: Coordinate -> BoundingBox -> Bool
isInside (x, y) ((minX, minY), (maxX, maxY)) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

step :: GridM Bool
step = do
  (pos, dir) <- viewState
  obstacles <- ask
  let pos' = walk dir pos
  boundingBox <- askBoundingBox
  let remainsInside = pos' `isInside` boundingBox
  when remainsInside $
    if pos' `S.member` obstacles
      then turnRight
      -- walkForward
      else modify $ (:) (pos', dir)
  return remainsInside

askBoundingBox :: GridM BoundingBox
askBoundingBox = do
  obstacles <- ask
  let bottomRight = calcSize obstacles
  pure ((0, 0), bottomRight)

hasLooped :: GridM Bool
hasLooped = do
  positions <- get
  case positions of
    [] -> pure False
    x : xs -> pure $ x `elem` xs

execGrid :: GridM a -> Input -> [(Coordinate, Direction)]
execGrid m (!obstacles, !start) = execState (runReaderT m obstacles) [(start, North)]

evalGrid :: GridM a -> Input -> [Coordinate]
evalGrid m = map fst . execGrid m

runGrid :: GridM a -> Set Coordinate -> [(Coordinate, Direction)] -> a
runGrid m !obstacles = evalState (runReaderT m obstacles)

whileM :: (Monad m) => m Bool -> m ()
whileM m = do
  b <- m
  when b $ whileM m

untilM :: (Monad m) => m Bool -> m () -> m ()
untilM p m = do
  b <- p
  unless b $ m >> untilM p m

solve1 :: Input -> Int
solve1 = length . nubOrd . evalGrid (whileM step)

turnRight :: GridM ()
turnRight = do
  (pos, dir) <- viewState
  let dir' = rotate dir
  modify ((pos, dir') :)

viewState :: GridM (Coordinate, Direction)
viewState = head <$> get

hasLoop :: GridM Bool
hasLoop = do
  (pos, dir) <- viewState
  obstacles <- ask
  let pos' = walk dir pos
  boundingBox <- askBoundingBox
  if pos' `isInside` boundingBox
    then do
      if pos' `S.member` obstacles
        -- turnRight
        then do
          turnRight
          loop <- hasLooped
          if loop
            then return True
            else hasLoop
        -- walkForward
        else do
          modify $ (:) (pos', dir)
          hasLoop
    else return False

solve2 :: (Set Coordinate, Coordinate) -> Int
solve2 (!obstacles, !start) = length . nubOrd . map fst . filter snd . map go $ paths
 where
  path :: [(Coordinate, Direction)] = execGrid (whileM step) (obstacles, start)
  paths :: [[(Coordinate, Direction)]] = init . init $ tails path
  go :: [(Coordinate, Direction)] -> (Coordinate, Bool)
  go ((pos, _) : xs) = (pos, notElem pos (map fst xs) && runGrid hasLoop (pos `S.insert` obstacles) xs)
  go [] = error "empty list"

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print input
  print $ solve1 input
  print $ solve2 input
