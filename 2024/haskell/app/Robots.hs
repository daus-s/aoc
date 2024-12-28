{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.List.Split (splitOn)
import Debug.Trace
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (iterate)

type Robot = (P, V)

type P = (Int, Int)

type V = (Int, Int)

height = 103

width = 101

main = do
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run robots -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let robots = map parseRobot (lines contents)
  let finalState = map (iterateN 100) robots
  let product = foldr (*) 1 (countQuadrants finalState)
  printf "the safety score after 100s is %d.\n" product
  printf "the minimum security factor (thus the xmas tree, the proof is trivial and left as an exercise to the reader) is after %d iterations.\n" (minIndex (securityFactors robots 10000))
  printf "the first temporal instance of multiple straight lines is at %d iterations.\n" (temporalInstance robots)

parseRobot :: String -> Robot
parseRobot s = (p, v)
  where
    v = (read $ velos !! 0, read $ velos !! 1)
    velos = splitOn "," velNP
    velNP = (splitOn "v=" vel) !! 1
    p = (read $ poses !! 0, read $ poses !! 1)
    poses = splitOn "," posNP
    posNP = (splitOn "p=" pos) !! 1
    vel = x !! 1
    pos = x !! 0
    x = splitOn " " s

iterate :: Robot -> Robot
iterate ((x, y), (vx, vy)) = (((x + vx) `mod` width, (y + vy) `mod` height), (vx, vy))

iterateN :: Int -> Robot -> Robot
iterateN 0 r = r
iterateN n r = iterateN (n - 1) (iterate r)

countQuadrants :: [Robot] -> [Int]
countQuadrants rs = [ul, ur, dl, dr]
  where
    ul = length $ filter (\((x, y), _) -> x < width `div` 2 && y < height `div` 2) rs
    ur = length $ filter (\((x, y), _) -> x > width `div` 2 && y < height `div` 2) rs
    dl = length $ filter (\((x, y), _) -> x < width `div` 2 && y > height `div` 2) rs
    dr = length $ filter (\((x, y), _) -> x > width `div` 2 && y > height `div` 2) rs

sf :: [Int] -> Int
sf = foldr (*) 1

securityFactors :: [Robot] -> Int -> [Int]
securityFactors rs 0 = []
securityFactors rs n = (sf $ countQuadrants rs) : securityFactors (map iterate rs) (n - 1)

minIndex :: [Int] -> Int
minIndex is = indexOf is (minimum is)

maxIndex :: [Int] -> Int
maxIndex is = indexOf is (maximum is)

indexOf :: [Int] -> Int -> Int -- 1 based index function (make sure to do the -1 when indexing.)
indexOf [] _ = -1
indexOf (i : is) t = case i == t of
  True -> 1
  False -> 1 + indexOf is t

hasVerticalLine :: [Robot] -> Bool
hasVerticalLine rs = any (\x -> x) (map (fiveContiguous rs) rs)
  where
    fiveContiguous :: [Robot] -> Robot -> Bool
    fiveContiguous rs ((x, y), v) =
      any (\x -> x) (map (isaPos (x, y + 1)) rs)
        && any (\x -> x) (map (isaPos (x, y + 2)) rs)
        && any (\x -> x) (map (isaPos (x, y + 3)) rs)
        && any (\x -> x) (map (isaPos (x, y + 4)) rs)
        && any (\x -> x) (map (isaPos (x, y + 5)) rs)

hasHorizontalLine :: [Robot] -> Bool
hasHorizontalLine rs = any (\x -> x) (map (fiveContiguous rs) rs)
  where
    fiveContiguous :: [Robot] -> Robot -> Bool
    fiveContiguous rs ((x, y), v) =
      any (\x -> x) (map (isaPos (x + 1, y)) rs)
        && any (\x -> x) (map (isaPos (x + 2, y)) rs)
        && any (\x -> x) (map (isaPos (x + 3, y)) rs)
        && any (\x -> x) (map (isaPos (x + 4, y)) rs)
        && any (\x -> x) (map (isaPos (x + 5, y)) rs)

isaPos :: P -> Robot -> Bool
isaPos (x, y) ((rx, ry), _) = x == rx && y == ry

temporalInstance :: [Robot] -> Int
temporalInstance rs =
  if hasHorizontalLine rs && hasVerticalLine rs
    then
      0
    else
      1 + temporalInstance (map iterate rs)