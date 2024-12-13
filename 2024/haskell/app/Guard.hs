{-# LANGUAGE InstanceSigs #-}

-- while this is unstable i determined it may be a good idea to use to leverage the efficient use  of indices in order to print

import Data.Maybe (fromMaybe)
import GHC.Arr (Array, array, bounds, indices, (!))
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (Left, Right)

data Map = Map [[Square]] deriving (Eq)

data Square = Obstacle | Tile Bool | Guard Direction -- obstacle, visited or a guard

data Direction = Up | Down | Left | Right

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
    Guard d1 -> case y of
      Guard d2 -> d1 == d2
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
  let arr = toMLL $ lines contents
  let finalCase = iterateMap (Map arr)
  printf "the guard visits %d distinct locations\n" (countVisited . ja $ finalCase)
  printf "does the given map loop: %s\n" (if doesLoop $ [Map arr] then "yes" else "no")

pmap :: Map -> String
pmap (Map arr) =
  unlines [join [psqr (arr @ (x, y)) | x <- [fst (fst confines) .. fst (snd confines)]] | y <- [snd (fst confines) .. snd (snd confines)]]
  where
    confines = (bounds $ toArray arr)

psqr :: Square -> String
psqr (Tile visited) = case visited of
  True -> "."
  False -> "o"
psqr (Guard x) = case x of
  Up -> "^"
  Down -> "v"
  Left -> "<"
  Right -> ">"
psqr (Obstacle) = "#"

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

-- tested
findGuardC :: Array (Int, Int) Char -> [(Int, Int)] -> (Int, Int)
findGuardC (arr) (x : xs) = case (arr ! x) of
  '>' -> x
  'v' -> x
  '<' -> x
  '^' -> x
  otherwise -> findGuardC arr xs

findGuard :: Array (Int, Int) Square -> [(Int, Int)] -> (Int, Int)
findGuard (arr) (x : xs) = case (arr ! x) of
  Guard _ -> x
  otherwise -> findGuard arr xs

toSquare :: Char -> Square
toSquare c = case c of
  '.' -> Tile False
  '#' -> Obstacle
  '>' -> Guard Right
  '<' -> Guard Left
  '^' -> Guard Up
  'v' -> Guard Down

-- mapped list of lists
toMLL :: [[Char]] -> [[Square]]
toMLL [] = []
toMLL (x : xs) = [mllRow x] ++ toMLL xs

mllRow :: [Char] -> [Square]
mllRow cs = map toSquare cs

join :: [String] -> String
join [] = ""
join (s : ss) = s ++ join ss

iterateMap :: Map -> Map
iterateMap (Map arr) = case arr @ (x, y) of
  Guard g -> case g of
    Up ->
      if inside arr (x, y - 1)
        then case arr @ (x, y - 1) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Right))) -- return same map with right facing guard
          Tile v -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True)) (x, y - 1) (Guard Up))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
    Right ->
      if inside arr (x + 1, y)
        then case arr @ (x + 1, y) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Down))) -- return same map with down facing guard
          Tile v -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True)) (x + 1, y) (Guard Right))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
    Down ->
      if inside arr (x, y + 1)
        then case arr @ (x, y + 1) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Left))) -- return same map with left facing guard
          Tile v -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True)) (x, y + 1) (Guard Down))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
    Left ->
      if inside arr (x - 1, y)
        then case arr @ (x - 1, y) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Up))) -- return same map with left facing guard
          Tile v -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True)) (x - 1, y) (Guard Left))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
  where
    (x, y) = findGuard grid (indices grid)
    grid = toArray arr

mapGeneration :: Map -> Maybe Map
mapGeneration (Map arr) = case arr @ (x, y) of
  Guard g -> case g of
    Up ->
      if inside arr (x, y - 1)
        then case arr @ (x, y - 1) of
          Obstacle -> Just $ (Map (modMap arr (x, y) (Guard Right))) -- return same map with right facing guard
          Tile v -> Just $ (Map (modMap (modMap arr (x, y) (Tile True)) (x, y - 1) (Guard Up))) -- return same map with the updated visit bool facing guard
        else
          Nothing
    Right ->
      if inside arr (x + 1, y)
        then case arr @ (x + 1, y) of
          Obstacle -> Just $ (Map (modMap arr (x, y) (Guard Down))) -- return same map with down facing guard
          Tile v -> Just $ (Map (modMap (modMap arr (x, y) (Tile True)) (x + 1, y) (Guard Right))) -- return same map with the updated visit bool facing guard
        else
          Nothing
    Down ->
      if inside arr (x, y + 1)
        then case arr @ (x, y + 1) of
          Obstacle -> Just $ (Map (modMap arr (x, y) (Guard Left))) -- return same map with left facing guard
          Tile v -> Just $ (Map (modMap (modMap arr (x, y) (Tile True)) (x, y + 1) (Guard Down))) -- return same map with the updated visit bool facing guard
        else
          Nothing
    Left ->
      if inside arr (x - 1, y)
        then case arr @ (x - 1, y) of
          Obstacle -> Just $ (Map (modMap arr (x, y) (Guard Up))) -- return same map with left facing guard
          Tile v -> Just $ (Map (modMap (modMap arr (x, y) (Tile True)) (x - 1, y) (Guard Left))) -- return same map with the updated visit bool facing guard
        else
          Nothing
  where
    (x, y) = findGuard grid (indices grid)
    grid = toArray arr

doesLoop :: [Map] -> Bool
doesLoop maps =
  case new of
    Just m ->
      if m `elem` maps
        then True
        else doesLoop $ maps ++ [m]
    Nothing -> False
  where
    new = mapGeneration $ last maps

(@) :: [[a]] -> (Int, Int) -> a
arr @ (x, y) = (arr !! (y - 1)) !! (x - 1)

modMap :: [[a]] -> (Int, Int) -> a -> [[a]]
modMap arr (i, j) val = (take y arr ++ [(take x row ++ [val] ++ drop (x + 1) row)] ++ drop (y + 1) arr)
  where
    row = (arr !! y)
    x = i - 1
    y = j - 1

printMapM_ :: [[Square]] -> IO ()
printMapM_ arr = mapM_ (putStrLn) (lines $ pmap (Map arr))

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
    (\y z -> (isVisited y) + z)
    0
    x
    + countVisited xs

isVisited :: Square -> Int
isVisited (Tile True) = 1
isVisited (Tile False) = 0
isVisited (Guard _) = 1
isVisited (Obstacle) = 0
