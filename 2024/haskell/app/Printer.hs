-- printer?
-- i hardly know her?

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Strings (Str (strSplitAll), strSplit)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run ceres -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let (rules, packets) = (map readRule $ lines x, map readPacket $ lines y) where (x, y) = strSplit "\n\n" contents
  print $ sum (map median (filter (isCompliant rules) packets))

-- print (map readPacket packets)

-- map . print $ (median . fromMaybe [])

median :: [a] -> a
median xs = xs !! middleNumber (length xs)

middleNumber :: Int -> Int
middleNumber x = floor (fromIntegral x / 2)

-- readPacket :: String -> Maybe [Int]
-- readPacket s = case all fromMaybe .  of

readPacket :: String -> [Int] -- (map (strSplitAll ",")
readPacket string = case all (\x -> x /= 0) (map readInt (strSplitAll "," string)) of
  False -> []
  True -> (map readInt (strSplitAll "," string))

readInt :: String -> Int
readInt x = case readMaybe x of
  Just a -> a
  Nothing -> 0

readRule :: String -> (Int, Int)
readRule s = (readInt x, readInt y) where (x, y) = strSplit "|" s

isCompliant :: [(Int, Int)] -> [Int] -> Bool
isCompliant rules packet = all (\x -> x) (map (isLegal packet) rules)

isLegal :: [Int] -> (Int, Int) -> Bool
isLegal list (pred, succ) = pred `notElem` ((singleSplit list succ) !! 1)

singleSplit :: [Int] -> Int -> [[Int]]
singleSplit [] _ = [[]]
singleSplit list val = [x !! 0] ++ [(concat (drop 1 x))] where x = splitOn [val] list

subjugate :: [Int] -> [(Int, Int)] -> [Int]
subjugate packet rules = [] -- TODO