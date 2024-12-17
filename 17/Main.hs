{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Bits (shiftR, xor, (.&.))
import Data.SBV (SWord64, Symbolic, constrain, minimize, sShiftRight, (./=), (.==))
import Data.SBV qualified as SBV
import Data.Void (Void)
import Data.Word (Word64)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String
type Output = [Word64]

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV
  deriving (Eq, Show, Ord, Enum, Bounded)

data ProgramState a = ProgramState
  { _registers :: (a, a, a)
  , _program :: UArray Int Word64
  , _pc :: Int
  , _output :: Output
  }

makeLenses ''ProgramState

registerP :: Parser Word64
registerP = do
  _ <- string "Register "
  _ <- char 'A' <|> char 'B' <|> char 'C'
  _ <- string ": "
  r <- L.decimal
  _ <- newline
  pure r

programP :: Parser (UArray Int Word64)
programP = do
  _ <- string "Program: "
  xs <- L.decimal `sepBy` char ','
  pure $ listArray (0, length xs - 1) xs

inputP :: Parser (ProgramState Word64)
inputP = do
  a <- registerP
  b <- registerP
  c <- registerP
  _ <- newline
  program <- programP
  pure $ ProgramState (a, b, c) program 0 []

type AppM = State (ProgramState Word64)

readLiteralOperand :: AppM Word64
readLiteralOperand = do
  pc' <- use pc
  program' <- use program
  let literal = program' ! (pc' + 1)
  pure $ literal `mod` 8

readComboOperand :: AppM Word64
readComboOperand = do
  pc' <- use pc
  program' <- use program
  (a, b, c) <- use registers
  let operand :: Word64 = program' ! (pc' + 1)
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
  pure . toEnum . fromIntegral $ program' ! pc'

registerA :: Lens' (ProgramState a) a
registerA = registers . _1

registerB :: Lens' (ProgramState a) a
registerB = registers . _2

registerC :: Lens' (ProgramState a) a
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
          pc .= fromIntegral operand
        else do
          pc += 2
    BXC -> do
      registerB .= b `xor` c
      pc += 2
    OUT -> do
      operand <- readComboOperand
      output %= (++ [operand `mod` 8])
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

solve1 :: ProgramState Word64 -> Output
solve1 = view output . execState runProgram

loop :: SWord64 -> Output -> Symbolic ()
loop a [] = constrain (a .== 0)
loop a (x : xs) = do
  constrain (a ./= 0)
  b <- pure (a .&. 7)
  b <- pure (b `xor` 1)
  c <- pure (a `sShiftRight` b)
  b <- pure (b `xor` 5)
  b <- pure (b `xor` c)
  constrain ((b .&. 7) .== fromIntegral x)
  a <- pure (a `shiftR` 3)
  loop a xs

-- 2,4 -> b = a mod 8
-- 1,1 -> b = b xor 1
-- 7,5 -> c = a div (2^b)
-- 1,5 -> b .= b `xor` 5
-- 4,2 -> b .= b `xor` c
-- 5,5 -> output (b mod 8)
-- 0,3 -> a `div` (2 ^ 3)
-- 3,0 -> jmp if a != 0 to beginning

solve2 :: ProgramState Word64 -> IO (Maybe Word64)
solve2 input =
  do
    res <- SBV.optLexicographic $ do
      a <- SBV.free "a" :: Symbolic SWord64
      minimize "smallest" a
      loop a (input ^. program . to elems)
    return $ SBV.getModelValue "a" res

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      -- print x
      print $ solve1 x
      a <- solve2 x
      print a
