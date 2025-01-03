{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.MemoTrie (memoFix)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
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

(∈) :: (Ord a) => a -> Set a -> Bool
(∈) = Set.member

join :: (Foldable t, Monoid a) => a -> t a -> a
join sep = foldr1 (\a b -> a <> sep <> b)

solve :: Input -> (Int, String)
solve input = (part1, part2)
 where
  part1 = length . filter (any startsWithT) $ cliques 3
  -- part2 is the last non-empty list of cliques
  part2 = join "," . head . last . takeWhile (not . null) $ cliques <$> [2 ..]
  startsWithT = ("t" `isPrefixOf`)
  computers = sort . nubOrd . (>>= (\(a, b) -> [a, b])) $ input
  edges :: Set Edge
  edges = Set.fromList . (>>= (\(a, b) -> [(a, b), (b, a)])) $ input
  cliques :: Int -> [NonEmpty Computer]
  cliques = memoFix $ \f k -> case k of
    1 -> pure <$> computers
    n -> [(x <| xs) | x <- computers, xs <- f (n - 1), x < NonEmpty.head xs, all (isConnected x) xs]
  isConnected :: Computer -> Computer -> Bool
  isConnected a b = (a, b) ∈ edges
  {-# INLINE isConnected #-}

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print $ solve x
