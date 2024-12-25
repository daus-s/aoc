import System.Environment (getArgs)
import Text.Printf (printf)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run reports -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let rows = lines contents
  let reports = map readReport rows
  let safe = sum (map countSafeEither reports)
  let maintained = sum (map withSafety reports)

  printf "number of safe reports: %d\n" safe
  printf "number of safe reports with backups: %d\n" maintained

safeDecline :: [Int] -> Bool
safeDecline (f : s : rest) = f - 1 >= s && f - 3 <= s && safeDecline (s : rest)
safeDecline (x : xs) = True -- suspect
safeDecline [] = True

safeIncline :: [Int] -> Bool
safeIncline (f : s : rest) = f + 1 <= s && f + 3 >= s && safeIncline (s : rest)
safeIncline (x : xs) = True -- suspect
safeIncline [] = True

readReport :: String -> [Int]
readReport x = map read (words x)

countSafeEither :: [Int] -> Int
countSafeEither x = if safeDecline x || safeIncline x then 1 else 0

withoutOne :: [Int] -> [[Int]]
withoutOne xs = map (removeIndex xs) [0 .. (length xs - 1)]

removeIndex :: [Int] -> Int -> [Int] -- this was the suspect
-- # learn how to read this
{-
  removeIndex, given:
    list
    index
  is defined as
    concatenate given
      left
      right
  from
    split the list given:
        index
      is defined as
        ( [left), [right] )

  (a) removeIndex, given a list and index is defined as
  (b) split the list at the index (with the left exclusive and the right inclusive),
  (c) then concatenate the lists given the tail of the right list and all of the left list

  aside: a proof that c is correct is left to the reader as an exercise
-}
removeIndex xs n = let (ys, zs) = splitAt n xs in ys ++ drop 1 zs

checkSafety :: [Int] -> Bool
checkSafety x = safeDecline x || safeIncline x

withSafety :: [Int] -> Int
withSafety x = if any checkSafety (withoutOne x) then 1 else 0