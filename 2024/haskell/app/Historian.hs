import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run historian -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let rows = lines contents
  let (lefts, rights) = makeList (map ((wrapUp . pair) . words) rows)
  print lefts
  print rights

absDiff :: (Int, Int) -> Int
absDiff (a, b) = if a > b then a - b else b - a

wrapUp :: Maybe (Int, Int) -> (Int, Int)
wrapUp = fromMaybe (- 10000000000, - 10000000000)

pair :: [String] -> Maybe (Int, Int)
pair ns = case length ns of
  2 -> Just (read (head ns), read (ns !! 1))
  _ -> Nothing

makeList :: [(Int, Int)] -> ([Int], [Int])
makeList [] = ([], [])
makeList (i : is) = (first i : first (makeList is), second i : second (makeList is))

first :: (a, b) -> a
first (x, y) = x

second :: (a, b) -> b
second (x, y) = y