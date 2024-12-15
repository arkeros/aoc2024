module Main where

import Control.Lens
import Control.Monad (forM, forM_, when)
import Control.Monad.State
import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Linear.V2
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Object = Wall | Box | Robot
  deriving (Eq, Show, Ord)

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

type Coordinates = V2 Int
type Input = (Map Coordinates Object, [Direction])
type Parser = Parsec Void String

dir :: Direction -> Coordinates
dir North = V2 (-1) 0
dir South = -dir North
dir East = V2 0 1
dir West = -dir East

directionP :: Parser Direction
directionP =
  choice
    [ North <$ string "^"
    , South <$ string "v"
    , West <$ string "<"
    , East <$ string ">"
    ]

generateArray :: String -> Array (Int, Int) (Maybe Object)
generateArray str = listArray ((0, 0), (n - 1, m - 1)) . concat $ rows
 where
  rows :: [[Maybe Object]] = fmap toObject <$> lines str
  n = length rows
  m = length . head $ rows
  toObject :: Char -> Maybe Object
  toObject '#' = Just Wall
  toObject '@' = Just Robot
  toObject 'O' = Just Box
  toObject '.' = Nothing
  toObject _ = error "Invalid character"

catMaybes2 :: [(a, Maybe b)] -> [(a, b)]
catMaybes2 = foldr f []
 where
  f (a, Just b) acc = (a, b) : acc
  f _ acc = acc

inputP :: Parser Input
inputP = do
  -- parse everything until the first 2 newlines
  str :: String <- manyTill anySingle (string "\n\n")
  let objects = M.fromList . map (first (uncurry V2)) . catMaybes2 . assocs . generateArray $ str
  directions <- some directionP `sepEndBy` newline
  pure (objects, mconcat directions)

swap :: (Coordinates, Coordinates) -> State (Map Coordinates Object) ()
swap (src, dest) = do
  mObj <- gets $ M.lookup src
  case mObj of
    Nothing -> error "No object to move"
    Just obj -> do
      modify $ M.delete src
      modify $ M.insert dest obj

move :: (Coordinates, Direction) -> State (Map Coordinates Object) Bool
move (pos, direction) = do
  objects <- get
  let pos' = pos + dir direction
  case M.lookup pos' objects of
    Just Wall -> pure False
    Just Box -> do
      moved <- move (pos', direction)
      when moved $ swap (pos, pos')
      pure moved
    Just Robot -> error "Robot should not be in the way"
    Nothing -> do
      swap (pos, pos')
      pure True

moveRobot :: Direction -> State (Coordinates, Map Coordinates Object) Bool
moveRobot !direction = do
  pos <- use _1
  moved <- zoom _2 $ move (pos, direction)
  when moved $ do
    let pos' = pos + dir direction
    _1 .= pos'
  pure moved

gps :: Coordinates -> Int
gps (V2 x y) = 100 * x + y

debug :: Input -> String
debug (!objectMap, dirs) = unlines . evalState moveAll $ initialState
 where
  initialState = (findRobot objectMap, objectMap)
  positions obj = ifolded . filtered (== obj) . asIndex
  findRobot = fromMaybe (error "No robot found") . preview (positions Robot)
  moveAll = forM dirs $ \dir -> do
    _ <- moveRobot dir
    objs <- use _2
    pure $ "Move " <> show dir <> ":\n" <> showMap objs <> "\n"

solve1 :: Input -> Int
solve1 (!objMap, dirs) = sumOf (_2 . positions Box . to gps) final
 where
  initial = (findRobot objMap, objMap)
  final = execState (forM_ dirs moveRobot) initial
  positions obj = ifolded . filtered (== obj) . asIndex
  findRobot = fromMaybe (error "No robot found") . preview (positions Robot)

solve2 :: Input -> Int
solve2 = undefined

showMap :: Map Coordinates Object -> String
showMap m =
  unlines
    [ [maybe '.' showObject $ M.lookup (V2 x y) m | y <- [0 .. maxY]]
    | x <- [0 .. maxX]
    ]
 where
  showObject :: Object -> Char
  showObject Wall = '#'
  showObject Box = 'O'
  showObject Robot = '@'
  maxX = maximum . map (view _x) $ M.keys m
  maxY = maximum . map (view _y) $ M.keys m

main :: IO ()
main = do
  input <- getContents
  case parse inputP "stdin" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right x -> do
      print x
      -- putStrLn $ debug x
      print $ solve1 x
