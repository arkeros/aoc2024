module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Void (Void)
import Linear.V2
import Linear.Vector ((*^))
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Position = V2 Int
type Velocity = V2 Int
type Robot = (Position, Velocity)
type Input = [Robot]
data Quadrant = Q1 | Q2 | Q3 | Q4
  deriving (Eq, Show, Ord)

type Parser = Parsec Void String

count :: (Ord a) => [a] -> [(a, Int)]
count = map ((,) <$> NE.head <*> length) . NE.group . sort

countBy :: (Ord b) => (a -> b) -> [a] -> [(b, Int)]
countBy f = count . map f

(^%^) :: (Integral a) => V2 a -> V2 a -> V2 a
(^%^) = liftA2 mod

vectorP :: Parser (V2 Int)
vectorP = V2 <$> signedDecimal <*> (string "," *> signedDecimal)
 where
  signedDecimal = L.signed (pure ()) L.decimal

robotP :: Parser Robot
robotP = (,) <$> (string "p=" *> vectorP) <*> (string " v=" *> vectorP)

inputP :: Parser Input
inputP = robotP `sepEndBy` some newline

bounds :: V2 Int
bounds = V2 101 103

walk :: Int -> Robot -> Position
walk t (p, v) = (p + (t *^ v)) ^%^ bounds

quadrant :: Position -> Maybe Quadrant
quadrant (V2 x y) = case (compare x 50, compare y 51) of
  (LT, LT) -> Just Q1
  (GT, LT) -> Just Q2
  (GT, GT) -> Just Q3
  (LT, GT) -> Just Q4
  _ -> Nothing

solve1 :: Input -> Int
solve1 = product . where_ isJust . countBy quadrant . map (walk 100)
 where
  where_ f = map snd . filter (f . fst)

solve2 :: [Robot] -> [(Int, [Robot])]
solve2 = takeWhile ((< period) . fst) . filter ((hasALongVertical <> hasLongHorizontal) . snd) . zip [0 ..] . iterate (map step)
 where
  step :: Robot -> Robot
  step = (,) <$> walk 1 <*> snd
  hasLong :: (Robot -> Int) -> [Robot] -> Bool
  hasLong f = any ((>= 20) . snd) . countBy f
  hasALongVertical = hasLong (^. _1 . _x)
  hasLongHorizontal = hasLong (^. _1 . _y)
  period = lcm (bounds ^. _x) (bounds ^. _y)

showRobots :: [Robot] -> String
showRobots = showMap . M.fromList . countBy fst
 where
  showMap :: M.Map Position Int -> String
  showMap m =
    unlines
      [ [maybe ' ' (const '#') $ M.lookup (V2 x y) m | x <- [0 .. 100]]
      | y <- [0 .. 102]
      ]

instance Semigroup Bool where
  (<>) = (&&)

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      print $ solve1 x
      forM_ (solve2 x) $ \(i, xs) -> do
        putStrLn $ "After " <> show i <> " seconds:"
        putStrLn $ showRobots xs
        threadDelay 100_000 -- 100 ms
