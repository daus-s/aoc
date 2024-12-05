import Data.Strings (Str (strPadBoth, strPadRight, strToLower))
import GHC.Arr (Array, array, bounds, indices, (!))
import System.Environment (getArgs)
import Text.Printf (printf)

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
  let rows = lines contents
  let grid = toArray rows
  printf "number of linear xmas's in grid: %d\n" (countSeq grid)
  printf "number of shaped xmas's in grid: %d\n" (countPat grid)

countPat :: Array (Int, Int) Char -> Int
countPat arr =
  foldr
    ( \i n ->
        boolToInt (isCrossMas arr i) + n
    )
    0
    (indices arr)

countSeq :: Array (Int, Int) Char -> Int
countSeq arr =
  foldr
    ( \i n ->
        (countLines arr i) + n
    )
    0
    (indices arr)

-- https://www.reddit.com/r/haskell/comments/loj3x7/2dimensional_algebraic_data_type/
toArray :: [[a]] -> Array (Int, Int) a
toArray vss =
  array
    ((1, 1), (w, h))
    [ ((x, y), v)
    | (y, vs) <- zip [1 ..] vss,
      (x, v) <- zip [1 ..] vs
    ]
  where
    w = case vss of
      [] -> 0
      vs : _ -> length vs
    h = length vss

countLines :: Array (Int, Int) Char -> (Int, Int) -> Int
countLines arr i =
  (boolToInt (isXmas (ul arr i 4)))
    + (boolToInt (isXmas (ur arr i 4)))
    + (boolToInt (isXmas (dl arr i 4)))
    + (boolToInt (isXmas (dr arr i 4)))
    + (boolToInt (isXmas (u arr i 4)))
    + (boolToInt (isXmas (d arr i 4)))
    + (boolToInt (isXmas (l arr i 4)))
    + (boolToInt (isXmas (r arr i 4)))

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

isBounded :: Int -> (Int, Int) -> Bool
isBounded x (l, u) = x >= l && x <= u

isBoundedBoth :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
isBoundedBoth (x, y) ((lx, ly), (ux, uy)) = isBounded x (lx, ux) && isBounded y (ly, uy)

ul :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
ul arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ ul arr (i - 1, j - 1) (l - 1)

ur :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
ur arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ ur arr (i + 1, j - 1) (l - 1)

dl :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
dl arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ dl arr (i - 1, j + 1) (l - 1)

dr :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
dr arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ dr arr (i + 1, j + 1) (l - 1)

u :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
u arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ u arr (i, j - 1) (l - 1)

d :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
d arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ d arr (i, j + 1) (l - 1)

l :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
l arr (i, j) x = case (x, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ l arr (i - 1, j) (x - 1)

r :: Array (Int, Int) Char -> (Int, Int) -> Int -> String
r arr (i, j) l = case (l, (isBoundedBoth (i, j) (bounds arr))) of
  (0, _) -> ""
  (_, False) -> ""
  (_, True) -> [((arr ! (i, j)))] ++ r arr (i + 1, j) (l - 1)

isXmas :: String -> Bool
isXmas x = x == "XMAS"

pp :: String -> String
pp s = case isXmas s of
  True -> strPadBoth ' ' 4 s
  False -> strToLower (strPadBoth ' ' 4 s) -- "...."

display :: Array (Int, Int) Char -> [String]
display grid =
  foldr
    ( \i n ->
        ( strPadRight
            ' '
            8
            ( show i
                ++ ": "
            )
            ++ "ul: "
            ++ pp (ul grid i 4)
            ++ " ur: "
            ++ pp (ur grid i 4)
            ++ " dl: "
            ++ pp (dl grid i 4)
            ++ " dr: "
            ++ pp (dr grid i 4)
            ++ " u: "
            ++ pp (u grid i 4)
            ++ " d: "
            ++ pp (d grid i 4)
            ++ " r: "
            ++ pp (r grid i 4)
            ++ " l: "
            ++ pp (l grid i 4)
        )
          : n
    )
    []
    (indices grid)

isCrossMas :: Array (Int, Int) Char -> (Int, Int) -> Bool
isCrossMas arr (x, y) = case isBorder arr (x, y) of
  True -> False
  False ->
    (arr ! (x, y) == 'A')
      && (arr ! (x - 1, y - 1) == 'M' && arr ! (x + 1, y + 1) == 'S' || arr ! (x - 1, y - 1) == 'S' && arr ! (x + 1, y + 1) == 'M')
      && (arr ! (x - 1, y + 1) == 'M' && arr ! (x + 1, y - 1) == 'S' || arr ! (x - 1, y + 1) == 'S' && arr ! (x + 1, y - 1) == 'M')

isBorder :: Array (Int, Int) Char -> (Int, Int) -> Bool
isBorder arr (x, y) = x == lx || x == ux || y == ly || y == uy where ((lx, ly), (ux, uy)) = bounds arr
