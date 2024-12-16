import System.Environment (getArgs)

data Node = Node Char Coord

data Coord = Coord Int Int

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
  printFieldM_ $ field

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