{-# LANGUAGE InstanceSigs #-}

-- while this is unstable i determined it may be a good idea to use to leverage the efficient use  of indices in order to print

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import GHC.Arr (Array, array, bounds, indices, (!))
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (Left, Right)

data Map = Map [[Square]] deriving (Eq)

data Square = Obstacle | Tile Knowledge -- obstacle, visited or a guard

data Guard = Guard Vector

data Knowledge = Knowledge Bool Bool Bool Bool -- up down left right

data Direction = Up | Down | Left | Right

data Vector = Vector Direction (Int, Int)

instance Eq Vector where
  (==) :: Vector -> Vector -> Bool
  (==) (Vector d1 (x1, y1)) (Vector d2 (x2, y2)) =
    d1 == d2 && x1 == x2 && y1 == y2

instance Eq Direction where
  (==) :: Direction -> Direction -> Bool
  (==) x y = case x of
    Up -> case y of
      Up -> True
      _ -> False
    Down -> case y of
      Down -> True
      _ -> False
    Left -> case y of
      Left -> True
      _ -> False
    Right -> case y of
      Right -> True
      _ -> False

instance Eq Square where
  (==) :: Square -> Square -> Bool
  (==) x y = case x of
    Obstacle -> case y of
      Obstacle -> True
      _ -> False
    Tile _ -> case y of
      Tile _ -> True
      _ -> False

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run guard -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let map = toMLL $ lines contents
  let guard = findGuardC $ lines contents
  let (finalMap, finalGuard) = iterateMap (Map map) guard

  printf "the guard visits %d distinct locations\n" (countVisited (ja finalMap))

  printf "there are %d squares that will result in a loop with an added obstacle\n" (obstification (Map map) (guard))

printMapM_ :: Map -> Guard -> IO ()
printMapM_ (Map arr) g = mapM_ (putStrLn) (lines $ pmap (Map arr) g)

pguard :: Guard -> String
pguard (Guard (Vector d (x, y))) = "" ++ pdir d ++ " @ (" ++ show x ++ ", " ++ show y ++ ")"

pdir :: Direction -> String
pdir d = case d of
  Up -> "^"
  Down -> "v"
  Left -> "<"
  Right -> ">"

pmap :: Map -> Guard -> String
pmap (Map arr) (Guard (Vector d (i, j))) =
  unlines [join [if x == i && y == j then pdir d else psqr (arr @ (x, y)) | x <- [fst (fst confines) .. fst (snd confines)]] | y <- [snd (fst confines) .. snd (snd confines)]]
  where
    confines = (bounds $ toArray arr)

psqr :: Square -> String
psqr (Tile (Knowledge u d l r)) = case u || d || l || r of
  True -> "."
  False -> "o"
psqr (Obstacle) = "#"

toMLL :: [[Char]] -> [[Square]]
toMLL [] = []
toMLL (x : xs) = [mllRow x] ++ toMLL xs

mllRow :: [Char] -> [Square]
mllRow cs = map toSquare cs

join :: [String] -> String
join [] = ""
join (s : ss) = s ++ join ss

-- utility
toArray :: [[a]] -> Array (Int, Int) a
toArray vss =
  array
    ((1, 1), (w, h))
    [ ((x, y), v)
    | (y, vs) <- zip [1 ..] vss,
      (x, v) <- zip [1 ..] vs
    ]
  where
    w = case vss of
      [] -> 0
      vs : _ -> length vs
    h = length vss

charDirection :: Char -> Direction
charDirection c = case c of
  '^' -> Up
  'v' -> Down
  '<' -> Left
  '>' -> Right

obstification :: Map -> Guard -> Int
obstification (Map arr) (Guard (Vector d (x, y))) = foldr (\a b -> if isLoop (Map (modMap arr a Obstacle)) (Guard (Vector d (x, y))) then 1 + b else b) 0 points
  where
    visits = filter (\x -> isVisited (arr @ x)) points
    points =
      ( [ (i, j)
        | i <-
            [ 1
              .. length
                ( case length arr of
                    0 -> []
                    _ -> arr !! 0
                )
            ],
          j <- [1 .. length (arr)]
        ]
      )

findGuardC :: [[Char]] -> Guard
findGuardC arr = case length guards of
  1 -> guards !! 0
  where
    guards =
      ( map
          ( \x -> case arr @ x of
              '>' -> Guard (Vector Right x)
              'v' -> Guard (Vector Down x)
              '<' -> Guard (Vector Left x)
              '^' -> Guard (Vector Up x)
          )
          ( filter
              ( \x -> case arr @ x of
                  '>' -> True
                  'v' -> True
                  '<' -> True
                  '^' -> True
                  otherwise -> False
              )
              points
          )
      )
    points =
      ( [ (i, j)
        | i <-
            [ 1
              .. length
                ( case length arr of
                    0 -> []
                    _ -> arr !! 0
                )
            ],
          j <- [1 .. length (arr)]
        ]
      )

toSquare :: Char -> Square
toSquare c = case c of
  '.' -> Tile (Knowledge False False False False)
  '#' -> Obstacle
  '>' -> Tile (Knowledge False False False True)
  '<' -> Tile (Knowledge False False True False)
  'v' -> Tile (Knowledge False True False False)
  '^' -> Tile (Knowledge True False False False)

turnRight :: Direction -> Direction -- this makes me think matrices...
turnRight d = case d of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

{-

In this example, our guard while oppositional is still polite. Before arriving at any tile
she lets the tile know the direction she enters that square in. Hmm, does this mean something weird for the input grid?
continue as planned, let the Tile that the guard jumps into know that its being visited

tell the tile that it is the spot of a turn, that is this square will have more than one visiting direction,

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----++#.
#+----++..
......#O..

in effect, where the pluses are.

-}

iterateMap :: Map -> Guard -> (Map, Guard)
iterateMap (Map arr) (Guard (Vector dir (x, y))) = case dir of
  Up ->
    if inside arr (x, y - 1)
      then case arr @ (x, y - 1) of
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y - 1) (Tile (Knowledge True d l r)))) (Guard (Vector (dir) (x, y - 1)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u d l True)))) (Guard (Vector (turnRight dir) (x, y)))
      else (Map arr, (Guard (Vector dir (x, y))))
  Right ->
    if inside arr (x + 1, y)
      then case arr @ (x + 1, y) of
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x + 1, y) (Tile (Knowledge u d l True)))) (Guard (Vector (dir) (x + 1, y)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u True l r)))) (Guard (Vector (turnRight dir) (x, y)))
      else (Map arr, (Guard (Vector dir (x, y))))
  Down ->
    if inside arr (x, y + 1)
      then case arr @ (x, y + 1) of
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y + 1) (Tile (Knowledge u True l r)))) (Guard (Vector (dir) (x, y + 1)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u d True r)))) (Guard (Vector (turnRight dir) (x, y)))
      else (Map arr, (Guard (Vector dir (x, y))))
  Left ->
    if inside arr (x - 1, y)
      then case arr @ (x - 1, y) of
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x - 1, y) (Tile (Knowledge u d True r)))) (Guard (Vector (dir) (x - 1, y)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge True d l r)))) (Guard (Vector (turnRight dir) (x, y)))
      else (Map arr, (Guard (Vector dir (x, y))))

isLoop :: Map -> Guard -> Bool
isLoop (Map arr) (Guard (Vector dir (x, y))) = case dir of
  Up ->
    if inside arr (x, y - 1)
      then case arr @ (x, y - 1) of
        Tile (Knowledge u d l r) -> u || isLoop (Map (modMap arr (x, y - 1) (Tile (Knowledge True d l r)))) (Guard (Vector (dir) (x, y - 1)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> r || isLoop (Map (modMap arr (x, y) (Tile (Knowledge u d l True)))) (Guard (Vector (turnRight dir) (x, y)))
      else
        False
  Right ->
    if inside arr (x + 1, y)
      then case arr @ (x + 1, y) of
        Tile (Knowledge u d l r) -> r || isLoop (Map (modMap arr (x + 1, y) (Tile (Knowledge u d l True)))) (Guard (Vector (dir) (x + 1, y)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> d || isLoop (Map (modMap arr (x, y) (Tile (Knowledge u True l r)))) (Guard (Vector (turnRight dir) (x, y)))
      else
        False
  Down ->
    if inside arr (x, y + 1)
      then case arr @ (x, y + 1) of
        Tile (Knowledge u d l r) -> d || isLoop (Map (modMap arr (x, y + 1) (Tile (Knowledge u True l r)))) (Guard (Vector (dir) (x, y + 1)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> l || isLoop (Map (modMap arr (x, y) (Tile (Knowledge u d True r)))) (Guard (Vector (turnRight dir) (x, y)))
      else
        False
  Left ->
    if inside arr (x - 1, y)
      then case arr @ (x - 1, y) of
        Tile (Knowledge u d l r) -> l || isLoop (Map (modMap arr (x - 1, y) (Tile (Knowledge u d True r)))) (Guard (Vector (dir) (x - 1, y)))
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> u || isLoop (Map (modMap arr (x, y) (Tile (Knowledge True d l r)))) (Guard (Vector (turnRight dir) (x, y)))
      else
        False

(@) :: [[a]] -> (Int, Int) -> a
arr @ (x, y) = (arr !! (y - 1)) !! (x - 1)

modMap :: [[a]] -> (Int, Int) -> a -> [[a]]
modMap arr (i, j) val = (take y arr ++ [(take x row ++ [val] ++ drop (x + 1) row)] ++ drop (y + 1) arr)
  where
    row = (arr !! y)
    x = i - 1
    y = j - 1

-- this function does assume a square/rectangle that is
-- returns boundaries (minX, minY), (maxX, maxY)
borders :: [[a]] -> ((Int, Int), (Int, Int))
borders (a : as) = ((1, 1), (length a, length (a : as)))

inside :: [[a]] -> (Int, Int) -> Bool
inside arr (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY where ((minX, minY), (maxX, maxY)) = borders arr

ja :: Map -> [[Square]]
ja (Map arr) = arr

countVisited :: [[Square]] -> Int
countVisited [] = 0
countVisited (x : xs) =
  foldr
    (\y z -> (if isVisited y then 1 else 0) + z)
    0
    x
    + countVisited xs

isVisited :: Square -> Bool
isVisited (Tile (Knowledge u d l r)) = if u || d || l || r then True else False
isVisited (Obstacle) = False
