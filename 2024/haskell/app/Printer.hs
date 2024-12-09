-- printer?
-- i hardly know her?

-- in all honesty, this problem was made far more difficult by my unwillingness to
-- conform to haskell's strong type system. several parts of the problem would
-- have required far less thinking. that is, less room to error in the code and
-- more difficult to have careless reading mistakes

import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Strings (Str (strSplitAll), strSplit)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run printer -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let (rules, packets) = (map readRule $ lines x, map readPacket $ lines y) where (x, y) = strSplit "\n\n" contents
  let corrected = map (subjugate (rules)) (filter (isTruant rules) packets)

  printf "the sum of the middle pages of all the correctly ordered packets is: %d\n" (sum (map median (filter (isCompliant rules) packets)))
  printf "the sum of the middle pages of all the corrected packets is: %d\n" (sum (map median corrected))

median :: [a] -> a
median xs = xs !! middleNumber (length xs)

middleNumber :: Int -> Int
middleNumber x = floor (fromIntegral x / 2)

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

isTruant :: [(Int, Int)] -> [Int] -> Bool
isTruant rules packet = not (isCompliant rules packet)

isLegal :: [Int] -> (Int, Int) -> Bool
isLegal list (pred, succ) = pred `notElem` ((singleSplit list succ) !! 1)

singleSplit :: [Int] -> Int -> [[Int]]
singleSplit [] _ = [[]]
singleSplit list val = [x !! 0] ++ [(concat (drop 1 x))] where x = splitOn [val] list

subjugate :: [(Int, Int)] -> [Int] -> [Int]
subjugate rules packet = generateList packet (filter (ruleApplies packet) rules)

ruleApplies :: [Int] -> (Int, Int) -> Bool
ruleApplies packet (pred, succ) = pred `elem` packet && succ `elem` packet

-- this should only be passed filtered rules for performance considerations
generateList :: [Int] -> [(Int, Int)] -> [Int]
generateList [] _ = []
generateList [x] rules = [x]
generateList (l : ls) rules = correctSplit ([], gist) rules l where gist = generateList ls rules

correctSplit :: ([Int], [Int]) -> [(Int, Int)] -> Int -> [Int]
correctSplit ([], []) rules val = [val]
correctSplit (start, []) rules val = start ++ [val]
correctSplit ([], (e : end)) rules val =
  case hasNoDeferrers (e : end) (getPredecessors val rules) of -- this is the initial case so this def should call itself
    True -> [val] ++ (e : end)
    False -> correctSplit ([e], end) rules val
-- first case to call self

correctSplit (start, e : end) rules val = case hasNoDeferrers start (getSuccessors val rules) && (hasNoDeferrers (e : end) (getPredecessors val rules)) of
  True -> start ++ [val] ++ (e : end)
  False -> correctSplit (start ++ [e], end) rules val

-- general recursive case

hasAny :: [Int] -> [Int] -> Bool
hasAny defers list = any (\x -> x) (map (isElem list) defers)

isElem :: [Int] -> Int -> Bool
isElem list val = val `elem` list

hasNoDeferrers :: [Int] -> [Int] -> Bool
hasNoDeferrers list defers = not (any (\x -> x) (map (`elem` list) defers))

getSuccessors :: Int -> [(Int, Int)] -> [Int]
getSuccessors val rules = map snd (filter (\x -> (fst x) == val) rules)

getPredecessors :: Int -> [(Int, Int)] -> [Int]
getPredecessors val rules = map fst (filter (\x -> (snd x) == val) rules)
