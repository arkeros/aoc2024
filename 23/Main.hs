{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sort)
import Data.MemoTrie (memoFix)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String
type Computer = String
type Input = [Edge]
type Edge = (Computer, Computer)

inputP :: Parser Input
inputP = edge `sepBy` newline
 where
  -- any string of 2 characters
  computer = count 2 alphaNumChar
  edge = (,) <$> computer <* char '-' <*> computer

-- solve2 :: Input -> Int
solve input = (part1, part2)
 where
  part1 = length . filter (any startsWithT) $ combinations 3
  -- part2 is the last non-empty list of combinations
  part2 = last . takeWhile (not . null) $ combinations <$> [2 ..]

  startsWithT = ("t" `isPrefixOf`)

  computers = sort . nubOrd . (>>= (\(a, b) -> [a, b])) $ input
  edgeSet :: Set Edge
  edgeSet = Set.fromList . (>>= (\(a, b) -> [(a, b), (b, a)])) $ input
  combinations :: Int -> [[Computer]]
  combinations = memoFix $ \f k -> case k of
    0 -> [[]]
    1 -> pure <$> computers
    n -> [x : xs | x <- computers, xs <- f (n - 1), x < head xs, all (isConnected x) xs]
  isConnected :: Computer -> Computer -> Bool
  isConnected a b = (a, b) `Set.member` edgeSet
  {-# INLINE isConnected #-}

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print $ solve x
