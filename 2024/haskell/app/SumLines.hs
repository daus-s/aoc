import System.Environment (getArgs)
import Text.Read (readMaybe)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run file-sum <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let rows = lines contents
  let (errors, results) = processRows rows
  mapM_ (putStrLn . formatError) errors
  mapM_ print results

-- Process each row, returning errors and valid results
processRows :: [String] -> ([String], [Int])
processRows = foldr process ([], [])
  where
    process line (errs, res) =
      case parseLine line of
        Just sum -> (errs, sum : res)
        Nothing -> (line : errs, res)

-- Parse a single line and return the sum if valid
parseLine :: String -> Maybe Int
parseLine line = do
  let tokens = words line
  nums <- mapM readMaybe tokens -- Safely parse integers
  return (sum nums)

-- Format an error message
formatError :: String -> String
formatError line =
  "Blech: bad input!"