module X2048
( Board
, prettyPrint
, move
, cpuMove
, freeIndexes
, generateRotations
, replace
, initialBoard
, addNewTile )
where

import Utils
import Data.IORef
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import System.Random (randomRIO)

type Board      = [Int]

-- L'hò fatta iò
initialBoard :: IO Board
initialBoard = (return blankBoard) >>= addNewTile >>= addNewTile
    where blankBoard = replicate 16 0

-- | Replaces element at IDX with newCell.
replace :: Int -> a -> [a] -> [a]
replace idx newCell list = take idx list ++ [newCell] ++ drop (idx+1) list

-- | Merges two cell of a row/column if they are equal whenever possible.
groupEquals :: [Int] -> [Int]
groupEquals [] = []
groupEquals [x] = [x]
groupEquals (xa:xb:xs)
    | xa == xb  = (xa+xa) : groupEquals xs
    | otherwise = xa : groupEquals (xb : xs)

-- | Moves the board to the left, merges cells and then returns the new matrix
mergeLeft :: [Board] -> [Board]
mergeLeft matrix   = map (fillWithZeros . groupEquals) removed
    where removed = map (filter (/=0)) matrix
          fillWithZeros l = l ++ replicate (4 - length l) 0 

-- | Matrix rotations functions
mirror = map reverse
rotl   = transpose . map reverse
rotr   = map reverse . transpose

move :: Board -> Direction -> Board
move board dir = case dir of
    Utils.Left  -> concat $ mergeLeft matrix
    Utils.Right -> concat $ mirror $ mergeLeft (mirror matrix)
    Up    -> concat $ rotr $ mergeLeft (rotl matrix)
    Down  -> concat $ rotl $ mergeLeft (rotr matrix)
  where matrix = chunksOf 4 board

generateRotations :: Board -> [ [Board] ]
generateRotations b = [board, mirror board, rotl board, rotr board]
    where board = chunksOf 4 b

-- | Pretty print of Board
prettyPrint :: Board -> IO ()
prettyPrint b = putStrLn $ intercalate "\n" $ map (intercalate "\t") (chunksOf 4 boardStr)
    where boardStr = map show b

-- | Returns the indexes of the empty cells.
freeIndexes :: Board -> [Int]
freeIndexes board = map snd freeTuples
    where indexes = zipWith (\x i->(x,i)) board [0..]
          freeTuples = filter (\(x,_)-> x==0) indexes

-- | Place a new tile.
cpuMove :: Board -> IO Board
cpuMove board = return board >>= addNewTile

-- | Given a Board, find an empty tile and fill it with 2 or 4.
addNewTile :: Board -> IO Board
addNewTile board = do
    let frees = freeIndexes board

    idx <- randomRIO (0, (length frees)-1) :: IO Int

    newCell <- randomRIO (1, 10) :: IO Int
    newCell <- return $ if (newCell == 1) then 4 else 2

    return $ replace (frees !! idx) newCell board
