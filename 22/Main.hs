module Main where

import Data.Bits (xor)
import Data.Function (on)
import Data.Int (Int8)
import Data.List (maximumBy, zipWith4)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Debug.Trace (trace)

type Input = [Word64]
type MonkeySequence = (Int, Int, Int, Int)

parseInput :: String -> Input
parseInput = map read . lines

prune :: Word64 -> Word64
prune n = n `mod` 16777216
{-# INLINE prune #-}

generateRandom :: Word64 -> Word64
generateRandom n = n'''
 where
  n' = prune $ (n * 64) `xor` n
  n'' = prune $ (n' `div` 32) `xor` n'
  n''' = prune $ (n'' * 2048) `xor` n''

solve1 :: Input -> Word64
solve1 = sum . map ((!! 2000) . iterate generateRandom)

prices :: Word64 -> [Int]
prices = take 2001 . map ((fromIntegral . (`mod` 10))) . iterate generateRandom

diffs :: (Integral a) => [a] -> [a]
diffs = zipWith (flip (-)) <*> tail

allMonkeys :: [MonkeySequence]
allMonkeys = [(a, b, c, d) | a <- [-9 .. 9], b <- [-9 .. 9], c <- [-9 .. 9], d <- [-9 .. 9]]

history :: Word64 -> [(MonkeySequence, Int)]
history secret = zip ms (drop 4 ps)
 where
  ps = prices secret
  ds = diffs ps
  ms :: [MonkeySequence] = zipWith4 (,,,) ds (drop 1 ds) (drop 2 ds) (drop 3 ds)

solve2 :: [Word64] -> (MonkeySequence, Int)
solve2 buyers = maximumBy (compare `on` snd) . Map.toList $ megamap
 where
  hs :: [Map MonkeySequence Int]
  hs = Map.fromList . reverse . history <$> buyers
  megamap = Map.unionsWith (+) hs

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ solve1 input
  -- print $ diffs . prices $ 123
  -- print $ history 2
  print $ solve2 input

-- print $ solve input
