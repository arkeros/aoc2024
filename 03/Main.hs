module Main where

import Control.Monad (forM_, when)
import Control.Monad.Combinators
import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Operation = Mul Int Int | Enable | Disable
  deriving (Show)

data Status = Enabled | Disabled
  deriving (Eq, Show)

type Input = [Operation]

type Parser = Parsec Void String

solve1 :: Input -> Int
solve1 = sum . map eval
 where
  eval :: Operation -> Int
  eval (Mul a b) = a * b
  eval _ = 0

type Program = State (Status, Int)

solve2 :: Input -> Int
solve2 xs = snd $ execState program (Enabled, 0)
 where
  program :: Program ()
  program = do
    forM_ xs $ \op -> do
      (status, acc) <- get
      case op of
        Mul a b -> when (status == Enabled) $ put (status, acc + a * b)
        Enable -> put (Enabled, acc)
        Disable -> put (Disabled, acc)

-- parse mul(X,Y) without spaces
mulP :: Parser Operation
mulP = do
  _ <- string "mul"
  _ <- char '('
  x <- decimal
  _ <- char ','
  y <- decimal
  _ <- char ')'
  pure $ Mul x y

enableP :: Parser Operation
enableP = Enable <$ string "do()"

disableP :: Parser Operation
disableP = Disable <$ string "don't()"

operationP :: Parser Operation
operationP = mulP <|> enableP <|> disableP

manySepByGarbage :: Parser a -> Parser [a]
manySepByGarbage p = do
  xs <- many (Just <$> try p <|> Nothing <$ anySingle)
  pure $ catMaybes xs

inputP :: Parser [Operation]
inputP = manySepByGarbage operationP

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right ops -> do
      print ops
      print $ solve1 ops
      print $ solve2 ops
