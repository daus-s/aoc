import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import GHC.Real (infinity)
import System.Environment (getArgs)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run practice -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let rows = lines contents
  --wtf is order of operations
  let s = sum $ map ((constructNum . unwrapNums) . extractPair) rows
  print s

extractPair :: String -> Maybe (Int, Int)
extractPair row = do
  first <- getFirstNum row
  last <- getLastNum row
  return (first, last)

getFirstNum :: String -> Maybe Int
getFirstNum [] = Nothing
getFirstNum (x : xs)
  | isDigit x = Just (digitToInt x)
  | otherwise = getFirstNum xs

getLastNum :: String -> Maybe Int
getLastNum = getFirstNum . reverse

constructNum :: (Int, Int) -> Int
constructNum (a, b) = (10 * a) + b

unwrapNum :: Maybe Int -> Int
unwrapNum = fromMaybe 1000000000

unwrapNums :: Maybe (Int, Int) -> (Int, Int)
unwrapNums (Just (a, b)) = (a, b)
unwrapNums Nothing = (1000000000, 1000000000)