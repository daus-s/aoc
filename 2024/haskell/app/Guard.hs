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
  let arr = toMLL $ lines contents
  let guard = findGuardC $ lines contents
  printMapM_ (Map arr) guard
  let finalCase = iterateMap (Map arr) guard

  -- mapM_ print (checkSquareList (Map arr))
  -- printf "the guard visits %d distinct locations\n" (countVisited . ja $ finalCase)
  -- printf "there are %d squares that will result in a loop with an added obstacle\n" (checkSquare (Map arr))
  -- printMapM_ $ (modMap arr (4, 7) Obstacle)
  -- print $ isLoop (Map (modMap arr (5, 9) Obstacle))
  -- print $ isLoop (Map (modMap arr (3, 9) Obstacle))
  putStrLn $ "___________________________________________________"
  -- printMapM_ $ iterateMap (Map $ modMap arr (5, 9) Obstacle) guard
  putStrLn $ "___________________________________________________"

-- printMapM_ $ ja $ iterateMap (Map $ modMap arr (3, 9) Obstacle)
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

-- mapped list of lists

-- iterateMap :: Map -> Map
-- iterateMap (Map arr) = case arr @ (x, y) of
--   Guard g -> case g of
--     Up ->
--       if inside arr (x, y - 1)
--         then case arr @ (x, y - 1) of
--           Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Right))) -- return same map with right facing guard
--           Tile (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u || True) (d || False) (l || False) (r || False)))) (x, y - 1) (Guard Up))) -- return same map with the updated visit bool facing guard
--         else
--           (Map arr)
--     Right ->
--       if inside arr (x + 1, y)
--         then case arr @ (x + 1, y) of
--           Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Down))) -- return same map with down facing guard
--           Tile (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u || False) (d || False) (l || False) (r || True)))) (x + 1, y) (Guard Right))) -- return same map with the updated visit bool facing guard
--         else
--           (Map arr)
--     Down ->
--       if inside arr (x, y + 1)
--         then case arr @ (x, y + 1) of
--           Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Left))) -- return same map with left facing guard
--           Tile (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u || False) (d || True) (l || False) (r || False)))) (x, y + 1) (Guard Down))) -- return same map with the updated visit bool facing guard
--         else
--           (Map arr)
--     Left ->
--       if inside arr (x - 1, y)
--         then case arr @ (x - 1, y) of
--           Obstacle -> iterateMap (Map (modMap arr (x, y) (Guard Up))) -- return same map with left facing guard
--           Tile (Knowledge u d l r) -> iterateMap (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (d) (True) (r)))) (x - 1, y) (Guard Left))) -- return same map with the updated visit bool facing guard
--         else
--           (Map arr)
--   where
--     (x, y) = findGuard grid (indices grid)
--     grid = toArray arr

turnRight :: Direction -> Direction
turnRight d = case d of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Right

iterateMap :: Map -> Guard -> (Map, Guard)
iterateMap (Map arr) (Guard (Vector dir (x, y))) = case dir of
  Up ->
    if inside arr (x, y - 1)
      then case arr @ (x, y - 1) of
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u d l True)))) (Guard (Vector (turnRight dir) (x, y)))
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y - 1) (Tile (Knowledge True d l r)))) (Guard (Vector (dir) (x, y - 1)))
      else (Map arr, (Guard (Vector dir (x, y))))
  Down ->
    if inside arr (x, y + 1)
      then case arr @ (x, y + 1) of
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u True l r)))) (Guard (Vector (turnRight dir) (x, y)))
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y + 1) (Tile (Knowledge u True l r)))) (Guard (Vector (dir) (x, y + 1)))
      else (Map arr, (Guard (Vector dir (x, y))))
  Left -> --TODO: Comoplete
    if inside arr (x - 1, y)
      then case arr @ (x - 1, y ) of
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u d l True)))) (Guard (Vector (turnRight dir) (x, y)))
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y - 1) (Tile (Knowledge True d l r)))) (Guard (Vector (dir) (x, y - 1)))
      else (Map arr, (Guard (Vector dir (x, y))))
  Right -> --TODO: Comoplete
    if inside arr (x, y + 1)
      then case arr @ (x, y + 1) of
        Obstacle -> case arr @ (x, y) of
          Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y) (Tile (Knowledge u True l r)))) (Guard (Vector (turnRight dir) (x, y)))
        Tile (Knowledge u d l r) -> iterateMap (Map (modMap arr (x, y + 1) (Tile (Knowledge u True l r)))) (Guard (Vector (dir) (x, y + 1)))
      else (Map arr, (Guard (Vector dir (x, y))))
-- isLoop :: Map -> Bool
-- isLoop (Map arr) = case arr @ (x, y) of
--   Guard dir -> case dir of
--     Up ->
--       if trace ("is " ++ show (x, y - 1) ++ " bounded? " ++ (if inside arr (x, y - 1) then "yes" else "no")) inside arr (x, y - 1)
--         then case arr @ (x, y - 1) of
--           Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Right))) -- return same map with right facing guard
--           Tile (Knowledge False d l r) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (True) (d) (l) (r)))) (x, y - 1) (Guard Up))) -- return same map with the updated visit bool facing guard
--           Tile (Knowledge True _ _ _) -> v && True
--         else
--           False
--     Right ->
--       if inside arr (x + 1, y)
--         then case arr @ (x + 1, y) of
--           Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Down))) -- return same map with down facing guard
--           Tile (Knowledge u d l False) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (d) (l) (True)))) (x + 1, y) (Guard Right))) -- return same map with the updated visit bool facing guard
--           Tile (Knowledge _ _ _ True) -> v && True
--         else
--           False
--     Down ->
--       if inside arr (x, y + 1)
--         then case arr @ (x, y + 1) of
--           Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Left))) -- return same map with left facing guard
--           Tile (Knowledge u False l r) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (True) (l) (r)))) (x, y + 1) (Guard Down))) -- return same map with the updated visit bool facing guard
--           Tile (Knowledge _ True _ _) -> v && True
--         else
--           False
--     Left ->
--       if inside arr (x - 1, y)
--         then case arr @ (x - 1, y) of
--           Obstacle -> isLoop (Map (modMap arr (x, y) (Guard Up))) -- return same map with upward facing guard
--           Tile (Knowledge u d False r) -> isLoop (Map (modMap (modMap arr (x, y) (Tile True (Knowledge (u) (d) (True) (r)))) (x - 1, y) (Guard Left))) -- return same map with the updated visit bool facing guard
--           Tile (Knowledge _ _ True _) -> v && True
--         else
--           False
--   where
--     (x, y) = findGuard grid (indices grid)
--     grid = toArray arr

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
    (\y z -> (isVisited y) + z)
    0
    x
    + countVisited xs

isVisited :: Square -> Int
isVisited (Tile (Knowledge u d l r)) = if u || d || l || r then 1 else 0
isVisited (Obstacle) = 0
