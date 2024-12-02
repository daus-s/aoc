import Data.Char (digitToInt, isDigit)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Printf (printf)

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
  let (lefts, rights) = doubleSort (makeList (map ((wrapUp . pair) . words) rows))
  let sm = sum (arrDiff lefts rights)
  let fr = sum (instanceCount lefts rights)
  printf "part 1 sum of distances: %d\n" sm
  printf "part 2 sum of counted instances: %d\n" fr

absDiff :: Int -> Int -> Int
absDiff a b = if a > b then a - b else b - a

doubleSort :: ([Int], [Int]) -> ([Int], [Int])
doubleSort (xs, ys) = (sort xs, sort ys)

wrapUp :: Maybe (Int, Int) -> (Int, Int)
wrapUp = fromMaybe (-10000000000, -10000000000)

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

--fun part of the homework!!!

arrDiff :: [Int] -> [Int] -> [Int]
arrDiff (l : ls) (r : rs) = absDiff l r : arrDiff ls rs
-- Found:
--   [absDiff l r] ++ arrDiff ls rs
-- Why not:
--   absDiff l r : arrDiff ls rs
-- hlint(refact:Use :)
-- arrDiff (l : ls) (r : rs) = [absDiff l r] ++ arrDiff ls rs

--base cases:
arrDiff (l : ls) [] = []
arrDiff [] (r : rs) = []
arrDiff [] [] = [] --dont try to get cutesy

instanceCount :: [Int] -> [Int] -> [Int]
instanceCount (l : ls) other = l * countInstances l other : instanceCount ls other
instanceCount _ other = []

countInstances :: Int -> [Int] -> Int
countInstances i = foldr (\x -> (+) (if i == x then 1 else 0)) 0

-- Equivalent: countInstances i (x : xs) = (if i == x then 1 else 0) + countInstances i xs
-- https://stackoverflow.com/questions/1757740/how-does-foldr-work