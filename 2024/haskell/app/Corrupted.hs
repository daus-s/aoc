import Data.List (isPrefixOf, stripPrefix)
import Data.Strings (Str (strSplit, strSplitAll))
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run corrupt -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let instructions = lines contents
  let products = sum . map tryParseNum $ instructionsToOperands $ contents
  printf "the product of the valid multiply instructions is: %d\n" products

-- transform text to code
instructionsToOperands :: String -> [String]
instructionsToOperands x = strSplitAll "mul(" x

tryParseNum :: String -> Int
tryParseNum x = case readTwo (strSplit "," (fst (strSplit ")" x))) of
  Just (a, b) -> a * b
  Nothing -> 0

readTwo :: (String, String) -> Maybe (Int, Int)
readTwo (left, right) = case (readInt left, readInt right) of
  (Just l, Just r) -> Just (l, r)
  (Just l, Nothing) -> Nothing
  (Nothing, Just r) -> Nothing
  (_, _) -> Nothing

readInt :: String -> Maybe Int
readInt x = readMaybe x