{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Debug.Trace (trace)
import System.Environment (getArgs)
import Text.Printf (printf)

data LinearEquation = LinEq A B

data System = Sys Prize Controls

data Prize = P Int Int

data Controls = Cl X Y

data A = A Int

data B = B Int

data X = X LinearEquation

data Y = Y LinearEquation

data Solution = Sol (A, B)

type Problem = String

instance Eq A where
  A x == A y = x == y

instance Eq B where
  B x == B y = x == y

instance Eq Solution where
  Sol (A a1, B b1) == Sol (A a2, B b2) = a1 == a2 && b1 == b2

instance Ord Solution where
  Sol (A a1, B b1) `compare` Sol (A a2, B b2) = ((3 * a1) + b1) `compare` ((3 * a2) + b2)

instance Show System where
  show (Sys (P solX solY) (Cl (X (LinEq (A ax) (B bx))) (Y (LinEq (A ay) (B by))))) = show solX ++ "=" ++ show ax ++ "a" ++ " + " ++ show bx ++ "b" ++ "\n" ++ show solY ++ "=" ++ show ay ++ "a" ++ " + " ++ show by ++ "b"

instance Show Solution where
  show (Sol (A a, B b)) = "a=" ++ show a ++ ", b=" ++ show b ++ "\n"

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run arcade -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let systems = map parse (groups contents)
  -- mapM_ print $ (map parse $ groups contents)
  printf "the minimum cost to attain all prizes is %d.\n" (sum $ map (tokens . solveSystem) systems)

groups :: String -> [Problem]
groups s = splitOn "\n\n" s

parse :: Problem -> System
parse p =
  Sys
    ( P
        px
        py
    )
    ( Cl
        (X $ LinEq (A ax) (B bx))
        (Y $ LinEq (A ay) (B by))
    )
  where
    by = read $ splitOn "Y+" bs !! 1
    bx = read $ splitOn "," (splitOn "X+" bs !! 1) !! 0
    ay = read $ splitOn "Y+" as !! 1
    ax = read $ splitOn "," (splitOn "X+" as !! 1) !! 0
    py = read $ splitOn "Y=" ps !! 1
    px = read $ splitOn "," (splitOn "X=" ps !! 1) !! 0
    as = (lines p) !! 0
    bs = (lines p) !! 1
    ps = (lines p) !! 2

solveSystem :: System -> Maybe Solution
solveSystem (Sys (P solX solY) (Cl (X (LinEq (A ax) (B bx))) (Y (LinEq (A ay) (B by))))) = case sols of
  [] -> Nothing
  xs -> Just $ minimum xs
  where
    sols =
      [ Sol (A a, B b)
      | a <- [0 .. 100],
        let (byQuo, byRem) = divMod (solY - ay * a) by,
        let (bxQuo, bxRem) = (solX - ax * a) `divMod` bx,
        let b = bxQuo,
        bxQuo == byQuo,
        byRem == 0,
        bxRem == 0
      ]

tokens :: Maybe Solution -> Int
tokens (Just (Sol (A a, B b))) = (3 * a) + b
tokens Nothing = 0