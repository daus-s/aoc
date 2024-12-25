import qualified Data.Set as S
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Printf (printf)

type Field = V.Vector (V.Vector Plot)

type Coord = (Int, Int)

type Plot = Char

data Facing = North | South | East | West

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run garden -- <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let field = chart contents
  printf "with 'modern' business practices the cost to fence the field is %d.\n" (cost $ regions field)

west :: Coord -> Coord
west (x, y) = (x - 1, y)

east :: Coord -> Coord
east (x, y) = (x + 1, y)

north :: Coord -> Coord
north (x, y) = (x, y - 1)

south :: Coord -> Coord
south (x, y) = (x, y + 1)

regions :: Field -> [S.Set Coord]
regions field = sub (indices field)
  where
    sub :: [Coord] -> [S.Set Coord]
    sub [] = []
    sub (c : cs) = res : sub (filter (\x -> not $ x `elem` res) cs)
      where
        res = contiguous field c

contiguous :: Field -> Coord -> S.Set Coord
contiguous field c = dfs c S.empty
  where
    dfs :: Coord -> S.Set Coord -> S.Set Coord
    dfs c s
      | c `S.member` s = s
      | otherwise =
          let s' = S.insert c s
              ns = Prelude.filter (\x -> isContained field x && field @ x == crop) cards
           in Prelude.foldr dfs s' ns
      where
        cards = [north c, south c, east c, west c]
        crop = field @ c

isBounded :: V.Vector a -> Int -> Bool
isBounded v i = i >= 0 && i < V.length v

isContained :: V.Vector (V.Vector a) -> Coord -> Bool
isContained v (x, y) = isBounded v y && isBounded (v V.! 0) x

(@) :: V.Vector (V.Vector a) -> (Int, Int) -> a
vec @ (x, y) = (vec V.! y) V.! x

chart :: String -> Field
chart s = V.fromList $ Prelude.map (\row -> processRow row) (lines s)

processRow :: String -> V.Vector Plot
processRow s = V.fromList s

pfield :: Field -> String
pfield f = Prelude.concatMap prow f

prow :: V.Vector Plot -> String
prow v = (Prelude.concatMap (\x -> [x]) v) Prelude.++ "\n"

indices :: Field -> [Coord]
indices field =
  [ (x, y)
  | x <- [0 .. length (field V.! 0) - 1],
    y <- [0 .. length field - 1]
  ]

area :: S.Set Coord -> Int
area = S.size

perimeter :: S.Set Coord -> Int
perimeter s = sum $ map countPresentNeighbors (S.elems s)
  where
    countPresentNeighbors :: Coord -> Int
    countPresentNeighbors c = length (filter (\x -> not $ x `elem` s) [north c, south c, east c, west c])

econ :: S.Set Coord -> Int
econ region = area region * perimeter region

cost :: [S.Set Coord] -> Int
cost s = sum $ map econ s