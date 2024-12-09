module Main where

import Control.Monad (forM_, liftM2, when)
import Control.Monad.ST
import Data.Function (on)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

type FileID = Int
type Block = Maybe FileID
type Input = [Int]

whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ cond m = do
  b <- cond
  when b $ m >> whileM_ cond m

indexedGroup :: (Eq a) => [a] -> [(a, NonEmpty Int)]
indexedGroup = map ((,) <$> snd . NE.head <*> fmap fst) . NE.groupWith snd . zip [0 ..]

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
solve1 = checksum . defrag . hydrate
 where
  defrag :: [Block] -> [Block]
  defrag xs = runST $ do
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
solve2 = checksum . defrag . hydrate
 where
  defrag :: [Block] -> [Block]
  defrag xs = runST $ do
    v <- V.thaw . V.fromList $ xs
    let (fs, ss) = partition (isJust . fst) . indexedGroup $ xs
        files :: [NonEmpty Int] = snd <$> fs
    spacesRef <- newSTRef (snd <$> ss)
    forM_ (reverse files) $ \file -> do
      spaces <- readSTRef spacesRef
      spaces' <- compact file spaces $ \space -> forM_ (NE.zip space file) (uncurry (MV.swap v))
      writeSTRef spacesRef spaces'
    V.toList <$> V.freeze v

  -- Find the first space that is larger than the file
  compact :: NonEmpty Int -> [NonEmpty Int] -> (NonEmpty Int -> ST s ()) -> ST s [NonEmpty Int]
  compact _ [] _ = pure []
  compact file xs@(space : rest) action
    -- We are looking after the file, so we can stop
    | file `isAfter` space = pure xs
    | otherwise = case (compare `on` NE.length) space file of
        -- We found a space that is exactly the same size as the file, so we can use it and delete it
        EQ -> action space >> pure rest
        -- We found a space that is larger than the file, so we can use it and split it
        GT -> do
          action space
          let space' = NE.fromList . NE.drop (NE.length file) $ space
          pure $ space' : rest
        -- Recursion step
        LT -> (space :) <$> compact file rest action

  isAfter xs = (> NE.head xs) . NE.head

main :: IO ()
main = do
  input <- parseInput <$> getContents
  -- print input
  print $ solve1 input
  print $ solve2 input
