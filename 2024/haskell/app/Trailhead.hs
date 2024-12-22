{-# LANGUAGE InstanceSigs #-}

import Data.Set (Set, empty, singleton, size, union)
import System.Environment (getArgs)

data Topography = Top [[Elevation]]

data Elevation = Elev Int

data Coord = Coord (Int, Int)

instance Show Topography where
  show :: Topography -> String
  show (Top x) = "+" ++ (concatMap (\_ -> "-") [1 .. length (x !! 0)]) ++ "+\n" ++ (concatMap (\y -> "|" ++ (concatMap show y) ++ "|\n") x) ++ "+" ++ (concatMap (\_ -> "-") [0 .. length (x !! 0)]) ++ "+\n"

instance Show Elevation where
  show :: Elevation -> String
  show (Elev i) = show i

instance Show Coord where
  show :: Coord -> String
  show (Coord (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Elevation where
  (==) :: Elevation -> Elevation -> Bool
  Elev x == Elev y = y == x

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  Coord (ax, ay) == Coord (bx, by) = ax == bx && ay == by

instance Ord Coord where
  compare :: Coord -> Coord -> Ordering
  (Coord a) `compare` (Coord b) =
    a `compare` b

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run trailhead -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let t = makeMap contents
  let zeros = getTrailheads t
  let paths = sum $ map size (map (findPaths t) zeros)
  print $ paths
  print $ t

makeMap :: String -> Topography
makeMap s = Top $ map row (lines s)

row :: String -> [Elevation]
row s = map toElevation s

-- assume safe input
toElevation :: Char -> Elevation
toElevation c = Elev $ read [c]

findPaths :: Topography -> Coord -> Set Coord
findPaths (Top grid) (Coord (x, y)) =
  case val of
    Elev 9 -> singleton (Coord (x, y))
    Elev v ->
      union
        ( union
            ( if (isBounded grid (Coord (x, y - 1)) && grid @ (x, y - 1) == Elev (v + 1))
                then
                  findPaths (Top grid) (Coord (x, y - 1))
                else empty
            )
            ( if isBounded grid (Coord (x, y + 1)) && (grid @ (x, y + 1) == Elev (v + 1))
                then
                  findPaths (Top grid) (Coord (x, y + 1))
                else empty
            )
        )
        ( union
            ( if (isBounded grid (Coord (x - 1, y)) && grid @ (x - 1, y) == Elev (v + 1))
                then
                  findPaths (Top grid) (Coord (x - 1, y))
                else empty
            )
            ( if isBounded grid (Coord (x + 1, y)) && (grid @ (x + 1, y) == Elev (v + 1))
                then
                  findPaths (Top grid) (Coord (x + 1, y))
                else empty
            )
        )
  where
    val = grid @ (x, y)

(@) :: [[a]] -> (Int, Int) -> a
arr @ (x, y) = (arr !! y) !! x

isBounded :: [[a]] -> Coord -> Bool
isBounded g (Coord (x, y)) = (length g) > y && y >= 0 && (length (g !! 0)) > x && x >= 0

getTrailheads :: Topography -> [Coord]
getTrailheads (Top grid) =
  concatMap
    ( \y ->
        concatMap (\x -> if grid @ (x, y) == (Elev 0) then [Coord (x, y)] else []) [0 .. length (grid !! y) - 1]
    )
    [0 .. ((length grid) - 1)]
