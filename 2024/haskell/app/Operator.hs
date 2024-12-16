import Data.List.Split (splitOn)
import Debug.Trace (trace)
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding ((||))

data Equation = Equation Int [Int] deriving (Show)

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run operator -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let eqs = map transcribe $ lines contents
  let valid = filter (\x -> isSolution x) eqs
  let validExp = filter (\x -> isExpandedSolution x) eqs
  printf "the sum of the possibly calibrated is %d\n" (eqSum valid)
  printf "the sum of the calibrated measurements with the full language is %d\n" (eqSum validExp)

transcribe :: String -> Equation
transcribe s = case length sides of
  2 ->
    Equation
      (readInt (sides !! 0))
      (map readInt (filter (\x -> x /= "") (splitOn " " (sides !! 1))))
  where
    -- otherwise error
    sides = splitOn ":" s

readInt :: String -> Int
readInt s = read s

getResults :: Equation -> [Int]
getResults (Equation _ [o]) = [o]
getResults (Equation r os) = concatMap (\x -> [(last os) * x, (last os) + x]) (getResults $ Equation r (init os))

getExpansions :: Equation -> [Int]
getExpansions (Equation _ [o]) = [o]
getExpansions (Equation r os) = concatMap (\x -> [(last os) * x, (last os) + x, x || (last os)]) (getExpansions $ Equation r (init os))

eqSum :: [Equation] -> Int
eqSum [] = 0
eqSum ((Equation r _) : rest) = r + eqSum rest

isSolution :: Equation -> Bool
isSolution (Equation r os) = r `elem` (getResults (Equation r os))

isExpandedSolution :: Equation -> Bool
isExpandedSolution (Equation r os) = r `elem` (getExpansions (Equation r os))

(||) :: Int -> Int -> Int
x || y = x * 10 ^ ((orderOfMagnitude y) + 1) + y

orderOfMagnitude :: Int -> Int
orderOfMagnitude n = if ((fromIntegral n) / 10.0) < 1 then 0 else 1 + (orderOfMagnitude (floor ((fromIntegral n) / 10)))