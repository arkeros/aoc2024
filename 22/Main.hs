module Main where

import Control.Parallel.Strategies (parMap, rpar, rseq)
import Data.Bits (xor)
import Data.Int (Int8)
import Data.List (zipWith4)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Debug.Trace (trace)

type Input = [Word64]
type MonkeySequence = (Int8, Int8, Int8, Int8)

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

prices :: Word64 -> [Int8]
prices = take 2001 . map (fromIntegral . (`mod` 10)) . iterate generateRandom

diffs :: [Int8] -> [Int8]
diffs = zipWith (flip (-)) <*> tail

allMonkeys :: [MonkeySequence]
allMonkeys = [(a, b, c, d) | a <- [-9 .. 9], b <- [-9 .. 9], c <- [-9 .. 9], d <- [-9 .. 9]]

maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn _ [] = error "maximumOn: empty list"
maximumOn f (x : xs) = g x (f x) xs
  where
    g v mv [] = v
    g v mv (x : xs)
        | mx > mv = g x mx xs
        | otherwise = g v mv xs
      where
        mx = f x

history :: Word64 -> [(MonkeySequence, Int8)]
history secret = zip ms (drop 4 ps)
  where
    ps = prices secret
    ds = diffs ps
    ms :: [MonkeySequence] = zipWith4 (,,,) ds (drop 1 ds) (drop 2 ds) (drop 3 ds)

solve2 :: Input -> (MonkeySequence, Int)
solve2 buyers = maximumOn snd $ map ((,) <$> id <*> bananas) allMonkeys
  where
    bananas :: MonkeySequence -> Int
    bananas monkey = sum . map (sellPrice monkey) $ hs
    hs :: [Map MonkeySequence Int8]
    hs = map (Map.fromList . reverse . history) buyers
    sellPrice :: MonkeySequence -> Map MonkeySequence Int8 -> Int
    sellPrice monkey = maybe 0 (fromIntegral) . Map.lookup monkey

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print $ solve1 input
    -- print $ diffs . prices $ 123
    -- print $ history 2
    print $ solve2 input

-- print $ solve input
