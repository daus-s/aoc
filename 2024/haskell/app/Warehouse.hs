{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.List.Split (splitOn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (Left, Right, iterate)

data Tile = Wall | Box | Empty | Robot

data Direction = Up | Down | Left | Right

type Map = Vector (Vector Tile)

type Instructions = [Direction]

type Coord = (Int, Int)

instance Show Tile where
  show Robot = "@"
  show Wall = "#"
  show Empty = "."
  show Box = "0"

pmap :: Map -> String
pmap m = Prelude.concatMap (\x -> ((Prelude.concatMap show x) Prelude.++ "\n")) m

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run warehouse -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let (warehouse, instructions) = parse contents
  putStrLn $ pmap warehouse
  putStrLn $ pmap $ iterate warehouse instructions
  printf "the sum of the goods positioning system of the warehouse is %d.\n" (scoreMap $ iterate warehouse instructions)

parse :: String -> (Map, Instructions)
parse s = (chart $ ss !! 0, says $ ss !! 1)
  where
    ss = splitOn "\n\n" s

says :: String -> Instructions
says s = map pd (concatMap (\x -> x) (lines s))
  where
    pd =
      ( \x -> case x of
          '>' -> Right
          '<' -> Left
          '^' -> Up
          'v' -> Down
      )

chart :: String -> Map
chart s = V.fromList $ map parseRow (lines s)
  where
    parseRow x = V.fromList $ map f x
    f x = case x of
      '#' -> Wall
      '.' -> Empty
      'O' -> Box
      '@' -> Robot

iterate :: Map -> Instructions -> Map
iterate m [] = m
iterate m (i : is) = iterate (moveSquad m ((from m (robot m) i) []) i) is

moveSquad :: Map -> [Coord] -> Direction -> Map
moveSquad m src d = foldl (replaceN) m (zip src dst)
  where
    dst = map df src
    df = case d of
      Up -> \(x, y) -> (x, y - 1)
      Down -> \(x, y) -> (x, y + 1)
      Left -> \(x, y) -> (x - 1, y)
      Right -> \(x, y) -> (x + 1, y)

from :: Map -> Coord -> Direction -> [Coord] -> [Coord]
from map c d xs =
  case map @ c of
    Box -> from map (cd c d) d (c : xs)
    Robot -> from map (cd c d) d (c : xs)
    Empty -> xs
    Wall -> []

cd :: Coord -> Direction -> Coord
cd (x, y) d = case d of
  Up -> (x, y - 1)
  Down -> (x, y + 1)
  Left -> (x - 1, y)
  Right -> (x + 1, y)

mcd :: Map -> Coord -> Direction -> Tile
mcd map (x, y) d = case d of
  Up -> map @ (x, y - 1)
  Down -> map @ (x, y + 1)
  Left -> map @ (x - 1, y)
  Right -> map @ (x + 1, y)

(@) :: Map -> Coord -> Tile
map @ (x, y) = (map V.! y) V.! x

robot :: Map -> Coord
robot map =
  ( filter
      ( \c -> case map @ c of
          Robot -> True
          _ -> False
      )
      is
  )
    !! 0
  where
    is = ([(x, y) | y <- [0 .. length map - 1], x <- [0 .. length (map V.! 0) - 1]])

replaceN :: Map -> (Coord, Coord) -> Map -- apply src then dst
replaceN m (src, dst) = replace (replace m dst (m @ src)) src Empty

replace :: Map -> Coord -> Tile -> Map
replace m (x, y) t = (V.take y m) V.++ V.singleton ((V.take (x) row) V.++ V.singleton t V.++ V.drop (x + 1) row) V.++ (V.drop (y + 1) m)
  where
    row = (m V.! y)

scoreMap :: Map -> Int
scoreMap m =
  sum
    ( map
        ( \(i, j) -> case m @ (i, j) of
            Box -> (100 * j + i)
            _ -> 0
        )
        idxs
    )
  where
    idxs = ([(x, y) | y <- [0 .. length m - 1], x <- [0 .. length (m V.! 0) - 1]])