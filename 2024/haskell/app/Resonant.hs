{-# LANGUAGE InstanceSigs #-}

import Data.Set (Set, empty, fromList, union)
import System.Environment (getArgs)
import Text.Printf (printf)

data Node = Node Char Coord deriving (Show)

data Coord = Coord Int Int deriving (Show, Eq)

instance Ord Coord where
  compare :: Coord -> Coord -> Ordering
  (Coord x1 y1) `compare` (Coord x2 y2) =
    if y1 == y2
      then
        x1 `compare` x2
      else
        y1 `compare` y2

data Field = Field [[Point]]

data Point = Point Node | Space

main = do
  -- Get the command-line arguments
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: cabal run resonant -- <filename>"

-- Open and read the file, process rows, and print the results for
processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let field = Field (toResField (lines contents) 0)
  let boundedAntinodes = findAntinodes field (nodes field)
  printf "there are %d antinodes bounded in the map" (length boundedAntinodes)

antinodes :: Node -> Node -> Coord
antinodes (Node _ (Coord x1 y1)) (Node _ (Coord x2 y2)) = Coord (x1 + (2 * (x2 - x1))) (y1 + 2 * (y2 - y1))

x :: Coord -> Int
x (Coord a _) = a

y :: Coord -> Int
y (Coord _ b) = b

pnt :: Char -> Int -> Int -> Point
pnt c x y = case c of
  '.' -> Space
  v -> (Point (Node v (Coord x y)))

toResField :: [String] -> Int -> [[Point]]
toResField [] h = []
toResField (s : ss) h = [(map (\x -> (pnt (s !! x) x h)) [0 .. (length s) - 1])] ++ (toResField ss (h + 1))

ppnt :: Point -> Char
ppnt p = case p of
  Point (Node val (Coord _ _)) -> val
  Space -> '.'

pfield :: Field -> [String]
pfield (Field arr) =
  map (\x -> join (map ppnt x)) arr

printFieldM_ :: Field -> IO ()
printFieldM_ (Field arr) = mapM_ (putStrLn) (pfield (Field arr))

join :: [Char] -> String
join [] = ""
join (s : ss) = [s] ++ join ss

nodes :: Field -> [Node]
nodes (Field field) =
  concatMap
    ( concatMap
        ( \m -> case pointToNode m of
            Just n -> [n]
            Nothing -> []
        )
    )
    field

getOtherLikeNodes :: Field -> Node -> [Node]
getOtherLikeNodes (Field field) (Node c (Coord x y)) =
  concatMap
    ( concatMap
        ( \p -> case pointToNode p of
            Just (Node oc (Coord ox oy)) ->
              if c == oc && x /= ox && y /= oy
                then
                  [Node oc (Coord ox oy)]
                else
                  []
            Nothing -> []
        )
    )
    field

borders :: Field -> ((Int, Int), (Int, Int))
borders (Field field) = case length field of
  0 -> ((0, 0), (0, 0))
  h -> ((0, 0), (length (field !! 0) - 1, h - 1))

inside :: ((Int, Int), (Int, Int)) -> Coord -> Bool
inside ((minx, miny), (maxx, maxy)) (Coord x y) = x >= minx && x <= maxx && y >= miny && y <= maxy

pointToNode :: Point -> Maybe Node
pointToNode p = case p of
  Point (Node c (Coord x y)) -> Just (Node c (Coord x y))
  Space -> Nothing

eqFreq :: Char -> Node -> Bool
eqFreq c (Node n _) = n == c

findAntinodes :: Field -> [Node] -> Set Coord
findAntinodes _ [] = empty
findAntinodes f (n : ns) = union (fromList (filter (inside (borders f)) (map (antinodes n) (getOtherLikeNodes f n)))) (findAntinodes f ns)