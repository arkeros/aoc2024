{-# LANGUAGE DerivingVia #-}

module Main where

import Control.Lens hiding (children)
import Data.List (permutations)
import Data.MemoTrie (memoFix)

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
dirFromVector _ = error "dirFromVector: invalid vector"

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

solve2 :: Input -> Int
solve2 = sum . map complexity
 where
  complexity :: String -> Int
  complexity = (*) <$> unDist . solveSeqDP 25 <*> numericPart
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

move' :: Coordinates -> DirectionalKeypad -> Coordinates
move' p (DirectionalButton dir) = move dir p
move' p ActivateButton = p

pathsFromVector :: Coordinates -> [[DirectionalKeypad]]
pathsFromVector v = map (|> ActivateButton) $ permutations (verticals <> horizontals)
 where
  verticals = map DirectionalButton $ replicate (abs (fst v)) (dirFromVector (signum (fst v), 0))
  horizontals = map DirectionalButton $ replicate (abs (snd v)) (dirFromVector (0, signum (snd v)))

-- Use dynamic programming to solve the sequence problem
solveSeqDP :: Int -> String -> Distance Int
solveSeqDP n str = sum . map (\(src, dst) -> dist (src, dst, 0)) $ (zip ((fromNumericButton 'A') : xs) xs)
 where
  xs = map fromNumericButton str

  -- Distance from src to target, at layer i
  dist :: (Coordinates, Coordinates, Int) -> Distance Int
  dist = memoFix $ \f (src, target, i) -> case (n == i) of
    True -> 1 + manhattan src target
    False -> if v == (0, 0) then 1 else minimum (d <$> paths)
     where
      v = target ^-^ src
      paths = filter isGoodPath . pathsFromVector $ target ^-^ src
      d :: [DirectionalKeypad] -> Distance Int
      d path = sum . map (\(p, q) -> f (fromDirectionalKeypad p, fromDirectionalKeypad q, i + 1)) $ zip (ActivateButton : path) path
      coords :: [DirectionalKeypad] -> [Coordinates]
      coords path = scanl move' src path
      isGoodPath :: [DirectionalKeypad] -> Bool
      isGoodPath = all (/= badCoord) . coords
      badCoord =
        if i == 0
          then (3, 0)
          else (0, 0)

main :: IO ()
main = do
  let input = ["869A", "170A", "319A", "349A", "489A"]
  print $ solve2 input