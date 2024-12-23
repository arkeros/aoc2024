{-# LANGUAGE DerivingVia #-}

module Main where

import Control.Lens hiding (children)
import Data.Array.Unboxed
import Data.Containers.ListUtils (nubOrd)
import Data.Graph
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (flatten, unfoldTree)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String
type Computer = String
type Input = [(Computer, Computer)]
type Key = Computer

inputP :: Parser Input
inputP = edge `sepBy` newline
 where
  -- any string of 2 characters
  computer = count 2 alphaNumChar
  edge = (,) <$> computer <* char '-' <*> computer

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = ((x :) <$> combinations (k - 1) xs) <> combinations k xs

triples :: [a] -> [(a, a, a)]
triples = map (\[a, b, c] -> (a, b, c)) . combinations 3

instance Semigroup Bool where
  (<>) = (&&)

-- solve :: Input -> (Int, Int)
-- solve :: [(Computer, Computer)] -> [[Computer]]
-- solve :: Input -> [(Computer, Computer, Computer)]
-- solve :: [(Computer, Computer)] -> Int
solve input = length . filter (hasT <> isInterconnected) . (>>= triples) . map (map keyFromVertex . flatten) $ components graph
 where
  -- Graph construction
  (graph, nodeFromVertex, vertexFromKey) =
    graphFromEdges [(isInteresting key, key, children key) | key <- computers]
  cellFromVertex = view _1 . nodeFromVertex
  keyFromVertex = view _2 . nodeFromVertex
  children :: Computer -> [Computer]
  children computer = [target | (src, target) <- input, src == computer]
  computers = nubOrd . (>>= (\(a, b) -> [a, b])) $ input
  isInteresting :: Computer -> Bool
  isInteresting ('t' : _) = True
  isInteresting _ = False
  edgeSet :: Set (Key, Key)
  edgeSet = Set.fromList input
  isInterconnected :: (Computer, Computer, Computer) -> Bool
  isInterconnected (a, b, c) =
    ((a, b) `Set.member` edgeSet || (b, a) `Set.member` edgeSet)
      && ((a, c) `Set.member` edgeSet || (c, a) `Set.member` edgeSet)
      && ((b, c) `Set.member` edgeSet || (c, b) `Set.member` edgeSet)
  hasT :: (Computer, Computer, Computer) -> Bool
  hasT (a, b, c) = any isInteresting [a, b, c]

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      -- let trie = foldr insertTrie mempty $ fst x
      -- print trie
      -- print $ consumeTrie trie "gbbr"
      print $ solve x
