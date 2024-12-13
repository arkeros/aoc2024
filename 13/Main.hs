module Main where

import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Button = (Int, Int)
type Prize = (Int, Int)
type Machine = (Button, Button, Prize)
type Input = [Machine]

type Parser = Parsec Void String

buttonP :: Parser Button
buttonP = do
  _ <- string "Button "
  _ <- char 'A' <|> char 'B'
  _ <- string ": X+"
  x <- L.decimal
  _ <- string ", Y+"
  y <- L.decimal
  pure (x, y)

prizeP :: Parser Prize
prizeP = do
  _ <- string "Prize: X="
  x <- L.decimal
  _ <- string ", Y="
  y <- L.decimal
  pure (x, y)

machineP :: Parser Machine
machineP = do
  buttonA <- buttonP <* newline
  buttonB <- buttonP <* newline
  prize <- prizeP
  pure (buttonA, buttonB, prize)

inputP :: Parser Input
inputP = machineP `sepEndBy` (some newline)

solveMachine :: Machine -> Maybe (Int, Int)
solveMachine ((a, a'), (b, b'), (c, c')) = if r == 0 then Just (x, y) else Nothing
 where
  (y, r) = (a' * c - a * c') `divMod` (a' * b - a * b')
  (x, 0) = (c - b * y) `divMod` a

cost :: (Int, Int) -> Int
cost (a, b) = a * 3 + b

solve1 :: Input -> Int
solve1 = sum . map cost . mapMaybe solveMachine

solve2 :: Input -> Int
solve2 = solve1 . map adjustMachine
 where
  offset = 10000000000000
  adjustMachine :: Machine -> Machine
  adjustMachine (buttonA, buttonB, (x, y)) = (buttonA, buttonB, (x + offset, y + offset))

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      print $ solve1 x
      print $ solve2 x
