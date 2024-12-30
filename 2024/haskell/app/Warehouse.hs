import System.Environment (getArgs)
import Text.Printf (printf)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run warehouse -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  printf contents