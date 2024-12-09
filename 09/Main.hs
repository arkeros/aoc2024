module Main where

import Control.Monad (forM_, liftM2, when)
import Control.Monad.ST
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

type FileID = Int
type Block = Maybe FileID
type Input = [Int]

safeHead = listToMaybe

whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ cond m = do
  b <- cond
  when b $ m >> whileM_ cond m

indexedGroup :: (Eq a) => [a] -> [(a, NonEmpty Int)]
indexedGroup = map ((,) <$> snd . NE.head <*> fmap fst) . NE.groupWith snd . zip [0 ..]

-- Replace the first occurrence of `a` with `a'` in the list
replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst a a' (x : xs)
  | x == a = a' : xs
  | otherwise = x : replaceFirst a a' xs

-- Remove from the list the first element that satisfies the predicate
removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst a (x : xs)
  | x == a = xs
  | otherwise = x : removeFirst a xs

parseInput :: String -> Input
parseInput = fmap (read . pure)

checksum :: [Block] -> Int
checksum = sum . zipWith (\a mb -> a * fromMaybe 0 mb) [0 ..]

hydrate :: Input -> [Block]
hydrate = go True 0
 where
  go :: Bool -> FileID -> [Int] -> [Block]
  go _ _ [] = []
  go True id_ (x : xs) = replicate x (Just id_) <> go False id_ xs
  go False id_ (x : xs) = replicate x Nothing <> go True (id_ + 1) xs

solve1 :: Input -> Int
solve1 = checksum . amphipod . hydrate
 where
  amphipod :: [Block] -> [Block]
  amphipod xs = runST $ do
    let n = length xs
    i <- newSTRef 0
    j <- newSTRef (n - 1)
    v <- V.thaw (V.fromList xs)
    whileM_ (liftM2 (<) (readSTRef i) (readSTRef j)) $ do
      i' <- readSTRef i
      ma <- MV.read v i'
      case ma of
        Just _ -> do
          modifySTRef' i (+ 1)
        Nothing -> do
          j' <- readSTRef j
          MV.swap v i' j'
          modifySTRef' j (subtract 1)
    V.toList <$> V.freeze v

solve2 :: Input -> Int
solve2 = checksum . amphipod . hydrate
 where
  amphipod :: [Block] -> [Block]
  amphipod xs = runST $ do
    v <- V.thaw . V.fromList $ xs
    let (fs, ss) = partition (isJust . fst) . indexedGroup $ xs
        files :: [NonEmpty Int] = snd <$> fs
    spaces' <- newSTRef (snd <$> ss)
    forM_ (reverse files) $ \file -> do
      -- find the first space that is larger than the file
      spaces <- readSTRef spaces'
      case safeHead . filter (isLargerThan file) . takeWhile (isBefore file) $ spaces of
        Just space -> do
          forM_ (NE.zip space file) (uncurry (MV.swap v))
          if NE.length space == NE.length file
            -- remove the space
            then modifySTRef' spaces' (removeFirst space)
            -- just make the space smaller
            else do
              let space' = NE.fromList . NE.drop (NE.length file) $ space
              modifySTRef' spaces' (replaceFirst space space')
        Nothing -> pure ()
    V.toList <$> V.freeze v
  isLargerThan xs = (>= NE.length xs) . NE.length
  isBefore xs = (< NE.head xs) . NE.head

main :: IO ()
main = do
  input <- parseInput <$> getContents
  -- print input
  print $ solve1 input
  print $ solve2 input
