{-# LANGUAGE InstanceSigs #-}

import System.Environment (getArgs)

data Topography = Top [[Elevation]]

data Elevation = Elev Int

instance Show Topography where
  show :: Topography -> String
  show (Top x) = "+" ++ (concatMap (\_ -> "-") [1 .. length (x !! 0)]) ++ "+\n" ++ (concatMap (\y -> "|" ++ (concatMap show y) ++ "|\n") x) ++ "+" ++ (concatMap (\_ -> "-") [0 .. length (x !! 0)]) ++ "+\n"

instance Show Elevation where
  show :: Elevation -> String
  show (Elev i) = show i

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
  print $ t

makeMap :: String -> Topography
makeMap s = Top $ map row (lines s)

row :: String -> [Elevation]
row s = map toElevation s

-- assume safe input
toElevation :: Char -> Elevation
toElevation c = Elev $ read [c]