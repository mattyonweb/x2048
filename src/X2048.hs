module X2048
( Board
, Direction
, fromString
, stringExample
, prettyPrint
, move
, cpuMove
, freeIndexes)
where 

import Data.List
import Data.IORef
import System.IO
import System.Random
import Control.Monad

type Board      = [Int]
type Direction  = String

stringExample :: String
-- stringExample = "....2.......4..."
stringExample = "...42224.228..28"

-- initialBoard :: IO Board
-- initialBoard = do
    -- i1 <- randomRIO (0, 15)
    -- v1 <- randomRIO (1, 2)
    -- i2 <- randomRIO (0, 15)
    -- v2 <- randomRIO (1, 2)

    -- let board = replicate 16 0
    -- return ()
    
    

-- | Converts a 16-chars string of {.248} into a Board 
fromString :: String -> Board
fromString = map (\c -> if (c=='.') then 0 else (read [c] :: Int))

-- | Splits a list into chunks of length n
chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

-- | Merges two cell of a row/column if they are equal whenever possible
groupEquals :: [Int] -> [Int]
groupEquals [] = []
groupEquals [x] = [x]
groupEquals (xa:xb:xs)
    | xa == xb  = (xa+xa) : groupEquals xs
    | otherwise = xa : groupEquals (xb : xs)

fillWithZeros :: [Int] -> [Int]
fillWithZeros l = l ++ replicate (4 - length l) 0

-- | Moves the board to the left, merges cells and then returns the new matrix
makeMove :: [Board] -> [Board]
makeMove matrix   = map (fillWithZeros . groupEquals) removed
    where removed = map (filter (/=0)) matrix

-- | Matrix rotations functions
rotl   = transpose . map reverse
rotr   = map reverse . transpose

-- | Moves the matrix in the selected direction 
move :: Board -> Direction -> Board
move board dir
    | dir == "LEFT"  = concat $ makeMove matrix
    | dir == "RIGHT" = concat $ mirror $ makeMove $ mirror matrix
    | dir == "UP"    = concat $ rotr $ makeMove $ rotl matrix
    | dir == "DOWN"  = concat $ rotl $ makeMove $ rotr matrix
        where mirror = map reverse
              matrix = chunks 4 board

-- | Pretty print of Board
prettyPrint :: Board -> IO ()
prettyPrint b = putStrLn $ intercalate "\n" $ map (intercalate "\t") (chunks 4 boardStr)
    where boardStr = map show b

-- | Returns the indexes of the empty cells.
freeIndexes :: Board -> [Int]
freeIndexes board = map snd freeTuples
    where indexes = zipWith (\x i->(x,i)) board [0..]
          freeTuples = filter (\(x,i)-> x==0) indexes

-- | Place a new cell, randomly chosen from 2 or 4
cpuMove :: IORef Board -> IO ()
cpuMove iorefBoard = do
    concreteBoard <- (readIORef iorefBoard)

    let frees = freeIndexes concreteBoard
    idx <- randomRIO (0, (length frees)-1) :: IO Int

    newCell <- randomRIO (1, 10) :: IO Int
    let newCell' = if (newCell == 1) then 4 else 2
    
    putStrLn "index frees:"
    print frees
    putStrLn "chosen index"
    print $ frees !! idx
    putStrLn "new val"
    print newCell'

    writeIORef iorefBoard (take (frees !! idx) concreteBoard ++ [newCell'] ++ drop ((frees !! idx)+1) concreteBoard)

    return ()
