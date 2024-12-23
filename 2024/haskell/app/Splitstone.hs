import Data.MemoTrie
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (iterate)

data Solution = Sol Int Int

instance Eq Solution where
  Sol value1 steps1 == Sol value2 steps2 = value1 == value1 && steps1 == steps2

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
  let initStones = ssri contents
  printf "after %s blinks there are %d stones\n" (show 25) (sum $ map (countDescendants 25) initStones)
  printf "after %s blinks there are %d stones" (show 75) (sum $ map (countDescendants 75) initStones)

iterateDepth :: [Int] -> Int -> [Int]
iterateDepth is 0 = is
iterateDepth is n = iterateDepth (iterate is) (n - 1)

countDescendants :: Int -> Int -> Int
countDescendants steps val = memo2 cf steps val
  where
    cf 0 _ = 1
    cf n i = sum (map (countDescendants (n - 1)) (applyRules i))

iterate :: [Int] -> [Int]
iterate is = concatMap applyRules is

applyRules :: Int -> [Int]
applyRules i =
  if i == 0
    then
      [1]
    else
      if (len i `mod` 2) == 0
        then
          split i
        else
          [i * 2024]

-- count digits in number
len :: Int -> Int
len n = if ((fromIntegral n) / 10.0) < 1 then 1 else 1 + (len (floor ((fromIntegral n) / 10)))

split :: Int -> [Int]
split i = [(i - r) `div` 10 ^ d, r]
  where
    r = i `mod` (10 ^ d)
    d = len i `div` 2

ssri :: String -> [Int]
ssri s = map read (words s)