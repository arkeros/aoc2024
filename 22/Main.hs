module Main where

import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..), xor)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (maximumBy, zipWith4)
import Data.Word (Word64)

type Input = [Word64]
type MonkeySequence = IntMap.Key

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

toMonkey :: Int -> Int -> Int -> Int -> MonkeySequence
toMonkey a b c d =
  ((a + 9) `shiftL` 15)
    .|. ((b + 9) `shiftL` 10)
    .|. ((c + 9) `shiftL` 5)
    .|. (d + 9)

fromMonkey :: MonkeySequence -> (Int, Int, Int, Int)
fromMonkey ms = (a, b, c, d)
 where
  a = (ms `shiftR` 15) .&. 0b11111 - 9
  b = (ms `shiftR` 10) .&. 0b11111 - 9
  c = (ms `shiftR` 5) .&. 0b11111 - 9
  d = ms .&. 0b11111 - 9

history :: Word64 -> [(MonkeySequence, Int)]
history secret = zip ms (drop 4 ps)
 where
  ps = prices secret
  ds = diffs ps
  ms :: [MonkeySequence] = zipWith4 toMonkey ds (drop 1 ds) (drop 2 ds) (drop 3 ds)

solve2 :: [Word64] -> ((Int, Int, Int, Int), Int)
solve2 buyers = first fromMonkey . maximumBy (compare `on` snd) . IntMap.toList $ megamap
 where
  hs :: [IntMap Int] = IntMap.fromList . reverse . history <$> buyers
  megamap = IntMap.unionsWith (+) hs

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ solve1 input
  -- print $ diffs . prices $ 123
  -- print $ history 2
  print $ solve2 input

-- print $ solve input
