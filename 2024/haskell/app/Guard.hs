import GHC.Arr (Array)
import System.Environment (getArgs)

data Map = Map (Array (Int, Int) (Square)) (Int, Int) -- deriving (Show)

data Square = Square Bool Bool | Guard -- obstacle, visited

data Guard = Up | Down | Left | Right

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run guard -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  print contents

-- iterateGrid :: Map -> Map
-- iterateGrid (Map arr (x,y )) = case arr ! (x,y) of
--     Guard -> case g of
--         Up -> case

-- -- show (Map ) = "map"
-- show (Square x y) = case sqr of
--   (Square True False) -> ""
