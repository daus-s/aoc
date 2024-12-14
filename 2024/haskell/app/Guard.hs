{-# LANGUAGE InstanceSigs #-}

-- while this is unstable i determined it may be a good idea to use to leverage the efficient use  of indices in order to print

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import GHC.Arr (Array, array, bounds, indices, (!))
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (Left, Right)

data Map = Map [[Square]] deriving (Eq)

data Square = Obstacle | Tile Bool Knowledge | Guard Direction -- obstacle, visited or a guard

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
    Tile _ _ -> case y of
      Tile _ _ -> True
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

  mapM_ print (checkSquareList (Map arr))
  printf "the guard visits %d distinct locations\n" (countVisited . ja $ finalCase)
  printf "there are %d squares that will result in a loop with an added obstacle\n" (checkSquare (Map arr))
  printMapM_ $ (modMap arr (4, 7) Obstacle)
  print $ isLoop (Map (modMap arr (5, 9) Obstacle))
  print $ isLoop (Map (modMap arr (3, 9) Obstacle))
  putStrLn $ "___________________________________________________"
  printMapM_ $ ja $ iterateMap (Map $ modMap arr (5, 9) Obstacle)
  putStrLn $ "___________________________________________________"
  printMapM_ $ ja $ iterateMap (Map $ modMap arr (3, 9) Obstacle)

pmap :: Map -> String
pmap (Map arr) =
  unlines [join [psqr (arr @ (x, y)) | x <- [fst (fst confines) .. fst (snd confines)]] | y <- [snd (fst confines) .. snd (snd confines)]]
  where
    confines = (bounds $ toArray arr)

psqr :: Square -> String
psqr (Tile visited _) = case visited of
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

findGuardV :: Map -> Vector
findGuardV (Map m) = case (m @ coord) of
  Guard x -> Vector x coord -- this seems incomplete but it should be understood that if the guard found is not a guard the program should kill itself
  where
    coord = (findGuard arr (indices arr))
    arr = toArray m

findGuard :: Array (Int, Int) Square -> [(Int, Int)] -> (Int, Int)
findGuard (arr) (x : xs) = case (arr ! x) of
  Guard _ -> x
  otherwise -> findGuard arr xs

toSquare :: Char -> Square
toSquare c = case c of
  '.' -> Tile False (Knowledge False False False False)
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
          Tile v (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u || True) (d || False) (l || False) (r || False)))) (x, y - 1) (Guard Up))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
    Right ->
      if inside arr (x + 1, y)
        then case arr @ (x + 1, y) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Down))) -- return same map with down facing guard
          Tile v (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u || False) (d || False) (l || False) (r || True)))) (x + 1, y) (Guard Right))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
    Down ->
      if inside arr (x, y + 1)
        then case arr @ (x, y + 1) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Left))) -- return same map with left facing guard
          Tile v (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u || False) (d || True) (l || False) (r || False)))) (x, y + 1) (Guard Down))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
    Left ->
      if inside arr (x - 1, y)
        then case arr @ (x - 1, y) of
          Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Up))) -- return same map with left facing guard
          Tile v (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (d) (True) (r)))) (x - 1, y) (Guard Left))) -- return same map with the updated visit bool facing guard
        else
          (Map arr)
  where
    (x, y) = findGuard grid (indices grid)
    grid = toArray arr

isLoop :: Map -> Bool
isLoop (Map arr) = case arr @ (x, y) of
  Guard dir -> case dir of
    Up ->
      if trace ("is " ++ show (x, y - 1) ++ " bounded? " ++ (if inside arr (x, y - 1) then "yes" else "no")) inside arr (x, y - 1)
        then case arr @ (x, y - 1) of
          Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Right))) -- return same map with right facing guard
          Tile v (Knowledge False d l r) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (True) (d) (l) (r)))) (x, y - 1) (Guard Up))) -- return same map with the updated visit bool facing guard
          Tile v (Knowledge True _ _ _) -> v && True
        else
          False
    Right ->
      if inside arr (x + 1, y)
        then case arr @ (x + 1, y) of
          Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Down))) -- return same map with down facing guard
          Tile v (Knowledge u d l False) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (d) (l) (True)))) (x + 1, y) (Guard Right))) -- return same map with the updated visit bool facing guard
          Tile v (Knowledge _ _ _ True) -> v && True
        else
          False
    Down ->
      if inside arr (x, y + 1)
        then case arr @ (x, y + 1) of
          Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Left))) -- return same map with left facing guard
          Tile v (Knowledge u False l r) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (True) (l) (r)))) (x, y + 1) (Guard Down))) -- return same map with the updated visit bool facing guard
          Tile v (Knowledge _ True _ _) -> v && True
        else
          False
    Left ->
      if inside arr (x - 1, y)
        then case arr @ (x - 1, y) of
          Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Up))) -- return same map with upward facing guard
          Tile v (Knowledge u d False r) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (d) (True) (r)))) (x - 1, y) (Guard Left))) -- return same map with the updated visit bool facing guard
          Tile v (Knowledge _ _ True _) -> v && True
        else
          False
  where
    (x, y) = findGuard grid (indices grid)
    grid = toArray arr

checkSquareList :: Map -> [(Int, Int)]
checkSquareList (Map arr) =
  map
    fst
    ( filter
        (\(coord, map) -> (isLoop map))
        pairings
    )
  where
    pairings =
      map
        ( \coord ->
            case arr @ coord of
              Guard _ -> (coord, Map arr)
              _ -> (coord, Map (modMap arr coord Obstacle))
        )
        ttc
    ttc = (filter (\coord -> if isVisited (ja (iterateMap (Map arr)) @ coord) == 1 then True else False) (indices $ toArray arr)) -- this just says only put boxes on squares we will traverse

checkSquare :: Map -> Int
checkSquare (Map arr) = sum (map (\x -> if x then 1 else 0) (map isLoop newMaps))
  where
    newMaps =
      ( map
          ( \coord ->
              case arr @ coord of
                Guard _ -> Map arr
                Obstacle -> Map arr
                _ -> trace ("updating coord " ++ show coord ++ " to be an obstacle") (Map (modMap arr coord Obstacle))
          )
          ttc
      )
    ttc = (filter (\coord -> if isVisited (ja (iterateMap (Map arr)) @ coord) == 1 then True else False) (indices $ toArray arr))

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
isVisited (Tile True _) = 1
isVisited (Tile False _) = 0
isVisited (Guard _) = 1
isVisited (Obstacle) = 0
