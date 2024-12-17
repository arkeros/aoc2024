{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.RWS
import Data.Array.Unboxed
import Data.Bits (xor)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String
type Output = [Int]

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
  deriving (Eq, Show, Ord, Enum, Bounded)

data ProgramState = ProgramState
  { _registers :: (Int, Int, Int)
  , _program :: UArray Int Int
  , _pc :: Int
  }
deriving instance Show ProgramState
makeLenses ''ProgramState

registerP :: Parser Int
registerP = do
  _ <- string "Register "
  _ <- char 'A' <|> char 'B' <|> char 'C'
  _ <- string ": "
  r <- L.decimal
  _ <- newline
  pure r

programP :: Parser (UArray Int Int)
programP = do
  _ <- string "Program: "
  xs <- L.decimal `sepBy` char ','
  pure $ listArray (0, length xs - 1) xs

inputP :: Parser ProgramState
inputP = do
  a <- registerP
  b <- registerP
  c <- registerP
  _ <- newline
  program <- programP
  pure $ ProgramState (a, b, c) program 0

type AppM = RWS () Output ProgramState

readLiteralOperand :: AppM Int
readLiteralOperand = do
  pc' <- use pc
  program' <- use program
  let literal :: Int = program' ! (pc' + 1)
  pure $ literal `mod` 8

readComboOperand :: AppM Int
readComboOperand = do
  pc' <- use pc
  program' <- use program
  (a, b, c) <- use registers
  let operand :: Int = program' ! (pc' + 1)
  case operand of
    4 -> pure a
    5 -> pure b
    6 -> pure c
    7 -> error "Invalid operand"
    literal -> pure literal

readOpcode :: AppM Instruction
readOpcode = do
  pc' <- use pc
  program' <- use program
  pure $ toEnum (program' ! pc')

registerA :: Lens' ProgramState Int
registerA = registers . _1

registerB :: Lens' ProgramState Int
registerB = registers . _2

registerC :: Lens' ProgramState Int
registerC = registers . _3

executeIntruction :: AppM ()
executeIntruction = do
  (a, b, c) <- use registers
  opcode <- readOpcode
  case opcode of
    ADV -> do
      operand <- readComboOperand
      registerA .= a `div` (2 ^ operand)
      pc += 2
    BXL -> do
      operand <- readLiteralOperand
      registerB .= b `xor` operand
      pc += 2
    BST -> do
      operand <- readComboOperand
      registerB .= operand `mod` 8
      pc += 2
    JNZ ->
      if a /= 0
        then do
          operand <- readLiteralOperand
          pc .= operand
        else do
          pc += 2
    BXC -> do
      registerB .= b `xor` c
      pc += 2
    OUT -> do
      operand <- readComboOperand
      tell [operand `mod` 8]
      pc += 2
    BDV -> do
      operand <- readComboOperand
      registerB .= a `div` (2 ^ operand)
      pc += 2
    CDV -> do
      operand <- readComboOperand
      registerC .= a `div` (2 ^ operand)
      pc += 2

isHalted :: AppM Bool
isHalted = do
  pc' <- use pc
  program' <- use program
  pure $ pc' > snd (bounds program')

runProgram :: AppM ()
runProgram = do
  halts <- isHalted
  unless halts $ do
    executeIntruction
    runProgram

solve1 :: ProgramState -> Output
solve1 = snd . evalRWS runProgram ()

solve2 :: ProgramState -> Int
solve2 input = head $ do
  a <- [0 ..]
  let program :: ProgramState = input & registerA .~ a
  _

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      print $ solve1 x
