{-# LANGUAGE DerivingVia #-}

module Main where

import Control.Parallel.Strategies (parMap, rpar, rseq)

import Data.List (isPrefixOf, sort)
import Data.MemoTrie (memoFix)
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
  patternsP = (sort) <$> (wordP `sepBy` string ", ")
  designsP = wordP `sepBy` newline

-- makeStripesP :: [Pattern] -> Parser [String]
-- makeStripesP xs = some patternP <* eof
--  where
--   patterns :: [Parser String] = (string) <$> xs
--   patternP :: Parser String = try (choice patterns)

solve1 :: Input -> Int
solve1 (patterns, designs) = length . filter (== True) . parMap rseq canBeFormed $ designs
 where
  canBeFormed :: Design -> Bool
  canBeFormed = memoFix $ \f str -> case str of
    [] -> True
    _ -> any (\prefix -> f (drop (length prefix) str)) candidates
     where
      candidates = filter (`isPrefixOf` str) patterns

solve :: Input -> (Int, Int)
solve (patterns, designs) = (,) <$> part1 <*> part2 $ parMap rseq canBeFormed $ designs
 where
  part1 = length . filter (> 0)
  part2 = sum

  canBeFormed :: Design -> Int
  canBeFormed = memoFix $ \f str -> case str of
    [] -> 1
    _ -> sum $ map (\prefix -> f (drop (length prefix) str)) candidates
     where
      candidates = filter (`isPrefixOf` str) patterns

-- canBeFormed [] = True
-- canBeFormed str = any (\prefix -> canBeFormed (drop (length prefix) str)) candidates
--  where
--   candidates = filter (`isPrefixOf` str) ps

-- solve :: Input -> Int
-- solve (patterns, designs) = length . filter matches $ designs
--  where
--   -- ps = reverse $ sortOn length patterns
--   stripesP = makeStripesP patterns
--   -- matches :: Design -> Bool
--   matches = isRight . parse stripesP "design"

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      -- print $ solve1 x
      print $ solve x
