{-# LANGUAGE InstanceSigs #-}

import Data.Char (chr)
import Data.List.Split (splitOn)
import Data.Vector
  ( Vector,
    any,
    drop,
    dropWhile,
    empty,
    filter,
    find,
    findIndex,
    fromList,
    length,
    singleton,
    splitAt,
    sum,
    takeWhile,
    (!),
    (++),
  )
import Debug.Trace (trace)
import Numeric ()
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (any, drop, dropWhile, filter, init, last, length, splitAt, sum, tail, take, takeWhile)

data Disk = Disk (Vector Block)

data Block = Block Int | Space

data Mode = File | Free

instance Show Block where
  show :: Block -> String
  show (Block i) = show (i `mod` 10)
  show Space = "."

instance Show Disk where -- TODO
  show :: Disk -> String
  show (Disk (ss)) = Prelude.concatMap show ss

instance Eq Block where
  (==) :: Block -> Block -> Bool
  (==) Space (Block _) = False
  (==) (Block _) Space = False
  (==) Space Space = True
  (==) (Block a) (Block b) = a == b

instance Ord Block where
  compare :: Block -> Block -> Ordering
  (Block a) `compare` (Block b) =
    a `compare` b
  (Block a) `compare` (Space) =
    a `compare` (-1)
  (Space) `compare` (Block b) =
    (-1) `compare` b

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
  let (disk) = fromStr contents File 0
  printf "the checksum of the reordered disk is %d.\n" (checkSum (fillFree (Disk disk) 0 1))
  print $ fillFree (Disk disk) 0 1
  putStrLn "0099811188827773336446555566.............."
  printf "the checksum of the optimized disk is %d.\n" (checkSum (optimizeStorage (Disk disk) (maxFile disk)))
  print $ (optimizeStorage (Disk disk) (maxFile disk))
  print $ Disk $ swapBlocks disk 2 40 2
  print $ fileSize (Disk disk) 0
  print $ fileSize (Disk disk) 1
  print $ fileSize (Disk disk) 2
  print $ fileSize (Disk disk) 3
  print $ fileSize (Disk disk) 4
  print $ fileSize (Disk disk) 5
  print $ fileSize (Disk disk) 6
  print $ fileSize (Disk disk) 7
  print $ fileSize (Disk disk) 8
  print $ fileSize (Disk disk) 9
  putStrLn "00992111777.44.333....5555.6666.....8888.."

fromStr :: String -> Mode -> Int -> Vector Block
fromStr [] _ _ = empty
fromStr (c : cs) mode id = case mode of
  File -> fromList [(Block id) | _ <- [1 .. (readInt c)]] Data.Vector.++ fromStr cs Free (id + 1)
  Free -> fromList [Space | _ <- [1 .. (readInt c)]] Data.Vector.++ fromStr cs File (id)

readInt :: Char -> Int
readInt c = read [c]

-- getLastFile :: Disk -> Int -> Int
-- getLastFile (Disk blocks) depth = case last blocks of
--   Block i -> depth
--   Space -> getLastFile (Disk $ init blocks) (depth + 1)

-- getFirstSpace :: Disk -> Int -> Int
-- getFirstSpace (Disk (b : locks)) depth = case b of
--   Block i -> getFirstSpace (Disk locks) (depth + 1)
--   Space -> depth

fillFree :: Disk -> Int -> Int -> Disk
fillFree (Disk disk) first last = case isContiguous (Disk disk) of -- trace (show disk) trace ("first: ") trace (show first) trace ("last: ") trace (show last)
  True -> (Disk disk)
  False -> case disk ! first of
    Space -> case disk ! ((length disk) - last) of
      Block i -> fillFree (swap disk first last) first last
      Space -> fillFree (Disk disk) first (last + 1)
    Block i -> fillFree (Disk disk) (first + 1) last

swap :: Vector Block -> Int -> Int -> Disk
swap v a b =
  Disk $
    prefix
      Data.Vector.++ singleton (v ! (length v - b))
      Data.Vector.++ (drop 1 inter) -- trace ("id to drop: ") trace (show $ Data.Vector.head inter)
      Data.Vector.++ singleton (Space)
      Data.Vector.++ drop 1 suffix
  where
    (inter, suffix) = case length rest of
      0 -> (empty, empty)
      x -> splitAt (x - b) (rest) -- trace ("swapping the ") trace (show prefix) trace (show rest)
    (prefix, rest) = splitAt a v -- trace (show a)

isContiguous :: Disk -> Bool
isContiguous (Disk blocks) =
  not
    ( any
        ( \x -> case x of
            Space -> False
            Block _ -> True
        )
        (afterSpace)
    )
  where
    (beforeSpace, afterSpace) =
      ( ( takeWhile
            ( \x -> case x of
                Space -> False
                Block _ -> True
            )
            blocks
        ),
        ( dropWhile
            ( \x -> case x of
                Space -> False
                Block _ -> True
            )
            blocks
        )
      )

checkSum :: Disk -> Int
checkSum (Disk blocks) = sum (fromList [(blockValue (blocks ! x)) * x | x <- [0 .. (length blocks) - 1]]) -- trace (((show (fileId (blocks ! x)))) Prelude.++ "*" Prelude.++ show x Prelude.++ "=" Prelude.++ show ((blockValue (blocks ! x)) * x))

blockValue :: Block -> Int
blockValue (Block i) = i
blockValue Space = 0

allocated :: Disk -> (Vector Block, Vector Block)
allocated (Disk blocks) =
  ( ( takeWhile
        ( \x -> case x of
            Space -> False
            Block _ -> True
        )
        blocks
    ),
    ( dropWhile
        ( \x -> case x of
            Space -> False
            Block _ -> True
        )
        blocks
    )
  )

fileId :: Block -> Int
fileId (Block i) = i
fileId Space = -1

fileSize :: Disk -> Int -> Int
fileSize (Disk disk) id =
  length
    ( filter
        ( \x -> case x of
            Block i -> i == id
            Space -> False
        )
        disk
    )

-- return the first index of a gap of sufficiently long length
findGaps :: Vector Block -> Int -> Int -> Int -> Int
findGaps disk startingIndex maxIndex length =
  case startingIndex < maxIndex of
    True ->
      if all
        ( \x -> case x of
            Space -> True
            Block _ -> False
        )
        (fst (splitAt length (snd (splitAt startingIndex blocks))))
        then
          startingIndex
        else
          findGaps disk (startingIndex + 1) maxIndex length
    False -> -1
  where
    blocks = fst (splitAt maxIndex disk)

optimizeStorage :: Disk -> Int -> Disk
optimizeStorage (Disk disk) id = case id of
  0 -> (Disk disk)
  i -> case findGaps disk 0 ind size of
    -1 -> optimizeStorage (Disk disk) (id - 1) -- no gaps
    x -> optimizeStorage (Disk $ swapBlocks disk size ind x) (id - 1)
  where
    size = (fileSize (Disk disk) id)
    ind = case findIndex (\b -> fileId b == id) disk of
      Just index -> index
      Nothing -> -1

-- this assumes
swapBlocks :: Vector Block -> Int -> Int -> Int -> Vector Block
swapBlocks blocks len src dst = prefix Data.Vector.++ file Data.Vector.++ beforeFile Data.Vector.++ gap Data.Vector.++ afterFile
  where
    (file, afterFile) = (splitAt len includesFile)
    (beforeFile, includesFile) = (splitAt (src - (len + dst)) withoutGap)
    (gap, withoutGap) = (splitAt len includesGap)
    (prefix, includesGap) = (splitAt dst blocks)

maxFile :: Vector Block -> Int
maxFile blocks = fileId (maximum blocks)