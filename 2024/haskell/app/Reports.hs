import System.Environment (getArgs)
import Text.Printf (printf)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run historian -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let rows = lines contents
  let reports = map readReport rows
  let safe = countSafeEither reports
  printf "is [7, 6, 4, 2, 1] safe? expect yes, received %s\n" (if safeDecline [7, 6, 4, 2, 1] then "yes" else "no")
  printf "is [1, 2, 3, 4, 5] safe? expect yes, received %s\n" (if safeIncline [1, 2, 3, 4, 5] then "yes" else "no")
  print safe

safeDecline :: [Int] -> Bool
safeDecline (x : xs) = x <= head xs + 1 && x >= head xs - 2 && safeDecline xs
safeDecline [_] = True
safeDecline [] = True

safeIncline :: [Int] -> Bool
safeIncline (x : xs) = x > head xs && x <= head xs + 3 && safeDecline xs
safeIncline [] = True

readReport :: String -> [Int]
readReport x = map read (words x)

countSafeEither :: [[Int]] -> Int
countSafeEither =
  foldr
    (\x -> (+) (if safeDecline x || safeIncline x then 1 else 0))
    0
