{-# LANGUAGE DerivingVia #-}

module Main where

import Control.Lens hiding (children)
import Data.Array.Unboxed
import Data.Bits (xor)
import Data.Graph
import Data.List (isPrefixOf, sort)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Cable = String
type InitialWire = (Cable, Bool)
type Connection = ((Cable, Cable), Gate, Cable)
type Input = ([InitialWire], [Connection])
type Parser = Parsec Void String

data Gate = And | Or | Xor | Literal Bool
  deriving (Eq, Show)

inputP :: Parser Input
inputP = (,) <$> initialWiresP <* newline <*> circuitP
 where
  boolP = choice [True <$ char '1', False <$ char '0']
  initialWiresP = initialWireP `sepEndBy` newline
  initialWireP = (,) <$> wireP <* string ": " <*> boolP
  wireP = some alphaNumChar
  circuitP = connectionP `sepEndBy` newline
  connectionP = (\in1 g in2 out -> ((in1, in2), g, out)) <$> wireP <* space <*> gateP <* space <*> wireP <* string " -> " <*> wireP
  gateP =
    choice
      [ And <$ string "AND"
      , Or <$ string "OR"
      , Xor <$ string "XOR"
      ]

type Key = Cable

bitToInt :: Bool -> Int
bitToInt True = 1
bitToInt False = 0
{-# INLINE bitToInt #-}

bitsToInt :: [Bool] -> Int
bitsToInt [] = 0
bitsToInt (x : xs) = bitToInt x + 2 * bitsToInt xs

myDFS :: Graph -> [Vertex] -> [Tree Vertex]
myDFS g vs0 = go vs0
 where
  go :: [Vertex] -> [Tree Vertex]
  go [] = []
  go (v : vs) = (Node v as) : bs
   where
    as = go (g ! v)
    bs = go vs

-- solve :: Input -> (Distance Int, Int)
solve :: Input -> Int
solve (initials, circuit) = bitsToInt (evalTree <$> myDFS graph outputs)
 where
  outputs = mapMaybe vertexFromKey . sort . filter startsWithZ . map (view _3) $ circuit

  startsWithZ = ("z" `isPrefixOf`)

  -- Graph construction
  (graph, nodeFromVertex, vertexFromKey) =
    graphFromEdges
      ( [(Literal value, input, []) | (input, value) <- initials]
          <> [(gate, out, [in2, in1]) | ((in1, in2), gate, out) <- circuit]
      )
  gateFromVertex = view _1 . nodeFromVertex
  keyFromVertex = view _2 . nodeFromVertex

  evalTree :: Tree Vertex -> Bool
  -- evalTree (Node v children) = trace (show $ (gateFromVertex v, keyFromVertex v, keyFromVertex . rootLabel <$> children)) $
  evalTree (Node v children) = case gateFromVertex v of
    And -> and (evalTree <$> children)
    Or -> or (evalTree <$> children)
    Xor -> (evalTree (children !! 0) `xor` evalTree (children !! 1))
    Literal b -> b

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      print $ solve x
