module Main where

import Data.Tree
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Equation = (Int, [Int])
type Input = [Equation]

type Parser = Parsec Void String

equationP :: Parser Equation
equationP = do
  a <- L.decimal
  _ <- char ':'
  _ <- char ' '
  b <- L.decimal `sepBy` char ' '
  pure (a, b)

inputP :: Parser Input
inputP = equationP `sepEndBy` newline

data Operation = Addition | Multiplication | Concatenation
  deriving (Show, Eq)

concatenate :: Int -> Int -> Int
concatenate x y = x * (10 ^ n) + y
 where
  n :: Int = floor $ log10 (fromIntegral y) + 1
  log10 :: Float -> Float
  log10 = logBase 10

apply :: Operation -> Int -> Int -> Int
apply Addition = (+)
apply Multiplication = (*)
apply Concatenation = concatenate

buildTree :: [Operation] -> [Int] -> Tree (Operation, Int)
buildTree ops xs = unfoldTree buildNode (Addition, xs, 0)
 where
  buildNode (op, [y], acc) = ((op, result), [])
   where
    result = (apply op) acc y
  buildNode (op, y : ys, acc) = ((op, result), map (,ys,result) ops)
   where
    result = (apply op) acc y
  buildNode (_, [], _) = error "empty list"

canCalibrate :: [Operation] -> Equation -> Bool
canCalibrate ops (test, xs) = foldTree p tree
 where
  tree = buildTree ops xs
  p (_, x) [] = x == test
  p (_, x) bs =
    if x > test
      -- prune, since values cannot decrease. (0 is not allowed)
      then False
      else or bs

solve1 :: Input -> Int
solve1 = sum . map fst . filter (canCalibrate [Addition, Multiplication])

solve2 :: Input -> Int
solve2 = sum . map fst . filter (canCalibrate [Addition, Multiplication, Concatenation])

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      -- putStrLn . drawTree . fmap show $ buildTree [Addition, Multiplication, Concatenation] [6, 8, 6, 15]
      print $ solve1 x
      print $ solve2 x
