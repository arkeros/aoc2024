{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Array (listArray, (!))
import Data.Char (ord)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String
type Pattern = String
type Design = String
type Input = ([Pattern], [Design])

wordP :: Parser String
wordP = many alphaNumChar

inputP :: Parser Input
inputP = (,) <$> patternsP <* (newline >> newline) <*> designsP
 where
  patternsP = (wordP `sepBy` string ", ")
  designsP = wordP `sepBy` newline

data Trie = Node !Bool (IntMap Trie)
  deriving (Eq, Show)

insertTrie :: String -> Trie -> Trie
insertTrie [] (Node _ m) = Node True m
insertTrie (c : cs) (Node b m) = Node b $ IntMap.insertWith (<>) (ord c) (insertTrie cs mempty) m

lookupTrie :: String -> Trie -> Bool
lookupTrie [] (Node b _) = b
lookupTrie (c : cs) (Node _ m) = case IntMap.lookup (ord c) m of
  Nothing -> False
  Just t -> lookupTrie cs t

-- Check if a Trie is a prefix of a string
isPrefixTrie :: String -> Trie -> Bool
isPrefixTrie [] (Node b _) = b
isPrefixTrie _ (Node True _) = True
isPrefixTrie (c : cs) (Node False m) = case IntMap.lookup (ord c) m of
  Nothing -> False
  Just t -> isPrefixTrie cs t

-- Given a string, consume the trie and return all possible suffixes
consumeTrie :: Trie -> String -> [String]
consumeTrie (Node False _) [] = []
consumeTrie (Node True _) [] = [""]
consumeTrie (Node True m) suffix = suffix : consumeTrie (Node False m) suffix
consumeTrie (Node False m) (c : cs) = case IntMap.lookup (ord c) m of
  Nothing -> []
  Just t -> consumeTrie t cs

instance Semigroup Trie where
  (<>) :: Trie -> Trie -> Trie
  (Node b m) <> (Node b' m') = Node (b || b') (IntMap.unionWith (<>) m m')

instance Monoid Trie where
  mempty :: Trie
  mempty = Node False IntMap.empty

solve :: Input -> (Int, Int)
solve (patterns, designs) = (,) <$> part1 <*> part2 $ map nWays $ designs
 where
  part1 = length . filter (> 0)
  part2 = sum

  trie = foldr insertTrie mempty patterns

  -- with dynamic programming, backwards calculate the number of ways to form a pattern
  -- with the given trie of prefixes
  nWays :: Design -> Int
  nWays str = arr ! 0
   where
    arr = listArray (0, length str) $ (map f [0 .. length str - 1]) <> [1]
    f i = sum . map (\j -> arr ! j) . map indexOfSuffix . consumeTrie trie $ drop i str
    indexOfSuffix suffix = length str - length suffix

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
