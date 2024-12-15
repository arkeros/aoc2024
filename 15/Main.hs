module Main where

import Control.Lens
import Control.Monad (forM, forM_, when)
import Control.Monad.State.Strict
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

data Object = Wall | Box | BigBox | Robot
  deriving (Eq, Show, Ord)

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

type Grid = Map Coordinates Object
type Coordinates = V2 Int
type Input = (Grid, [Direction])
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

swap :: (Coordinates, Coordinates) -> State Grid ()
swap (src, dest) = do
  mObj <- gets $ M.lookup src
  case mObj of
    Nothing -> pure ()
    Just obj -> do
      modify $ M.delete src
      modify $ M.insert dest obj

getObjAt :: Coordinates -> State Grid (Coordinates, Maybe Object)
getObjAt pos = do
  let leftPost = pos + dir West
  (l, x) <- (,) <$> gets (M.lookup leftPost) <*> gets (M.lookup pos)
  pure $
    if x == Nothing && l == Just BigBox
      then (leftPost, Just BigBox)
      else (pos, x)

blockingPos :: Coordinates -> Direction -> [Coordinates]
blockingPos pos West = [pos]
blockingPos pos East = [pos + dir East, pos]
blockingPos pos direction = [pos + dir East, pos]

move :: (Coordinates, Direction) -> State Grid Bool
move (pos, direction) = do
  (myPos, moi) <- getObjAt pos
  let pos' = pos + dir direction
  obj <-
    if moi == Just BigBox && direction == East
      then getObjAt (myPos + dir East + dir East)
      else getObjAt pos'
  case obj of
    (_, Just Wall) -> pure False
    (objPos, Just Box) -> do
      moved <- move (objPos, direction)
      when moved $ swap (pos, pos')
      pure moved
    (bigBoxPos, Just BigBox) -> do
      originalState <- get
      moved <- and <$> forM (blockingPos bigBoxPos direction) (move . (,direction))
      if moved
        then swap (pos, pos')
        else put originalState
      pure moved
    (_, Just Robot) -> error "Robot should not be in the way"
    (_, Nothing) -> do
      swap (pos, pos')
      pure True

moveRobot :: Direction -> State (Coordinates, Grid) Bool
moveRobot !direction = do
  pos <- use _1
  moved <- zoom _2 $ move (pos, direction)
  when moved $ do
    let pos' = pos + dir direction
    _1 .= pos'
  pure moved

debug :: Input -> String
debug (!objectMap, dirs) = unlines . evalState moveAll $ initialState
 where
  initialState = (findRobot objectMap, objectMap)
  positions obj = ifolded . filtered (== obj) . asIndex
  countBigBoxes :: Grid -> Int
  countBigBoxes = length . toListOf (positions BigBox)

  findRobot = fromMaybe (error "No robot found") . preview (positions Robot)
  moveAll = forM dirs $ \dir -> do
    nBigBoxes <- use (_2 . to countBigBoxes)
    _ <- moveRobot dir
    objs <- use _2
    let nBigBoxes' = countBigBoxes objs
    let msg = if nBigBoxes' /= nBigBoxes then "invariant violation!\n" else ""
    pure $ "Move " <> show dir <> ":\n" <> showMap objs <> "\n" <> msg

gps :: Coordinates -> Int
gps (V2 x y) = 100 * x + y

solve1 :: Input -> Int
solve1 (!objMap, dirs) = sumOf (_2 . positions Box . to gps) final
 where
  initial = (findRobot objMap, objMap)
  final = execState (forM_ dirs moveRobot) initial
  -- finds all the coordinates of a given type of object
  positions objType = ifolded . filtered (== objType) . asIndex
  findRobot = fromMaybe (error "No robot found") . preview (positions Robot)

scaleUp :: Grid -> Grid
scaleUp = M.fromList . (>>= enlarge) . M.toList
 where
  enlarge (V2 x y, Wall) = [(V2 x (2 * y), Wall), (V2 x (2 * y + 1), Wall)]
  enlarge (V2 x y, Box) = [(V2 x (2 * y), BigBox)]
  enlarge (V2 x y, obj) = [(V2 x (2 * y), obj)]

solve2 :: Input -> Int
solve2 (!objMap, dirs) = sumOf (_2 . positions BigBox . to gps) final
 where
  objMap' = scaleUp objMap
  initial = (findRobot objMap', objMap')
  final = execState (forM_ dirs moveRobot) initial
  -- finds all the coordinates of a given type of object
  positions objType = ifolded . filtered (== objType) . asIndex
  findRobot = fromMaybe (error "No robot found") . preview (positions Robot)

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
  showObject BigBox = '['
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
      -- putStrLn $ debug (first scaleUp x)
      print $ solve1 x
      print $ solve2 x
