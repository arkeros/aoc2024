module Main where

import Control.Monad (forM_)
import Data.List
import Data.Monoid (Endo (..))
import Data.Traversable (for)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Void (Void)
import GHC.Real qualified as V
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data PageOrderingRule = GoesBefore Int Int
  deriving (Show)

type Page = Int
type Update = V.Vector Page
type Input = ([PageOrderingRule], [Update])

type Parser = Parsec Void String

orderingRuleP :: Parser PageOrderingRule
orderingRuleP = do
  a <- L.decimal
  _ <- char '|'
  b <- L.decimal
  pure $ GoesBefore a b

inputP :: Parser Input
inputP = do
  orderingRules <- orderingRuleP `sepEndBy` newline
  _ <- newline
  updates <- (V.fromList <$> L.decimal `sepBy` comma) `sepBy` newline
  pure (orderingRules, updates)
 where
  comma = char ','

-- Returns the middle element of a list
mid :: V.Vector a -> a
mid xs = xs V.! (V.length xs `div` 2)

follows :: Update -> PageOrderingRule -> Bool
follows update (GoesBefore a b) = case (a `V.elemIndex` update, b `V.elemIndex` update) of
  (Just i, Just j) -> i < j
  _ -> True

isOrderedWith :: [PageOrderingRule] -> Update -> Bool
isOrderedWith rules update = all (update `follows`) rules

solve1 :: Input -> Int
solve1 (rules, updates) = sum (mid <$> filter (isOrderedWith rules) updates)

makeObey :: PageOrderingRule -> Update -> Update
makeObey (GoesBefore a b) update = case (a `V.elemIndex` update, b `V.elemIndex` update) of
  (Just i, Just j) ->
    if i < j
      then update
      else
        V.modify
          ( \v -> do
              MV.write v j a
              forM_ [j + 1 .. i] $ \k -> MV.write v k (update V.! (k - 1))
          )
          update
  _ -> update

applyUntilNoChange :: (Eq a) => (a -> a) -> a -> a
applyUntilNoChange f x = until (\y -> f y == y) f x

solve2 :: Input -> Int
solve2 (rules, updates) = sum (mid . sort' <$> filter (not . isOrderedWith rules) updates)
 where
  reorder :: Update -> Update
  reorder = appEndo $ mconcat (Endo . makeObey <$> rules)
  sort' = applyUntilNoChange reorder

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      print $ solve1 x
      print $ solve2 x
