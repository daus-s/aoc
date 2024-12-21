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
    slice,
    splitAt,
    sum,
    take,
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
  printf "the checksum of the optimized disk is %d.\n" (checkSum (optimizeStorage (Disk disk) (maxFile disk)))

fromStr :: String -> Mode -> Int -> Vector Block
fromStr [] _ _ = empty
fromStr (c : cs) mode id = case mode of
  File -> fromList [(Block id) | _ <- [1 .. (readInt c)]] Data.Vector.++ fromStr cs Free (id + 1)
  Free -> fromList [Space | _ <- [1 .. (readInt c)]] Data.Vector.++ fromStr cs File (id)

readInt :: Char -> Int
readInt c = read [c]

fillFree :: Disk -> Int -> Int -> Disk
fillFree (Disk disk) first last =
  if first < last
    then case isContiguous (Disk disk) of -- trace (show disk) trace ("first: ") trace (show first) trace ("last: ") trace (show last)
      True -> (Disk disk)
      False -> case disk ! first of
        Space -> case disk ! ((length disk) - last) of
          Block i -> fillFree (Disk $ swap disk first last) first last
          Space -> fillFree (Disk disk) first (last + 1)
        Block i -> fillFree (Disk disk) (first + 1) last
    else
      (Disk disk)

swap :: Vector Block -> Int -> Int -> Vector Block
swap blocks dst src =
  prefix Data.Vector.++ file Data.Vector.++ beforeFile Data.Vector.++ space Data.Vector.++ afterFile
  where
    prefix = take dst blocks
    file = slice src 1 blocks
    beforeFile = slice (dst + 1) (src - (dst + 1)) blocks
    space = slice dst 1 blocks
    afterFile = drop (src + 1) blocks

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
findGaps disk startingIndex maxIndex len =
  case startingIndex < maxIndex of
    True ->
      if all
        ( \x -> case x of
            Space -> True
            Block _ -> False
        )
        ( slice
            startingIndex
            len
            disk
        )
        then
          startingIndex
        else
          findGaps disk (startingIndex + 1) maxIndex len
    False -> -1
  where
    blocks = fst (splitAt maxIndex disk)

optimizeStorage :: Disk -> Int -> Disk
optimizeStorage (Disk blocks) id = case id of -- trace (show id Prelude.++ " @ " Prelude.++ show src)
  0 -> (Disk blocks)
  i -> case findGaps blocks 0 src size of
    (-1) -> optimizeStorage (Disk blocks) (id - 1) -- no gaps
    x -> optimizeStorage (Disk $ swapBlocks blocks size src x) (id - 1) -- trace ("file " Prelude.++ show id Prelude.++ ": src@(" Prelude.++ show src Prelude.++ ")\nto dst: " Prelude.++ show x Prelude.++ "\nassert empty: " Prelude.++ show (slice x size blocks) Prelude.++ " (" Prelude.++ show (assertEmpty (slice x size blocks)) Prelude.++ ")\n") trace (show $ Disk blocks)
  where
    size = (fileSize (Disk blocks) id)
    src = case findIndex (\b -> fileId b == id) blocks of
      Just index -> index
      Nothing -> -1

-- this assumes
swapBlocks :: Vector Block -> Int -> Int -> Int -> Vector Block
swapBlocks blocks len src dst = prefix Data.Vector.++ file Data.Vector.++ beforeFile Data.Vector.++ space Data.Vector.++ afterFile -- trace ("file:" Prelude.++ (show $ file)) trace ("beforeFile:" Prelude.++ (show $ beforeFile)) trace ("space:" Prelude.++ (show $ space)) trace ("afterFile:" Prelude.++ (show $ afterFile)) trace (show $ Disk $ prefix Data.Vector.++ file Data.Vector.++ beforeFile Data.Vector.++ space Data.Vector.++ afterFile)
  where
    prefix = take dst blocks
    file = slice src len blocks
    beforeFile = slice (dst + len) (src - (dst + len)) blocks
    space = slice dst len blocks
    afterFile = drop (src + len) blocks

maxFile :: Vector Block -> Int
maxFile blocks = fileId (maximum blocks)

assertEmpty :: Vector Block -> Bool
assertEmpty =
  all
    ( \x -> case x of
        Space -> True
        Block i -> False
    )
