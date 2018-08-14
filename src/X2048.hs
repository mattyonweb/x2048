module X2048
( Board
, fromString
, stringExample
, prettyPrint
, move
, cpuMove)
where 

import Data.List
import Data.IORef
import System.IO
import System.Random
import Control.Monad

type Board      = [Int]
type Direction  = String

stringExample :: String
stringExample = "....2.......4..."

fromString :: String -> Board
fromString = map (\c -> if (c=='.') then 0 else (read [c] :: Int))

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

groupEquals :: [Int] -> [Int]
groupEquals [] = []
groupEquals [x] = [x]
groupEquals (xa:xb:xs)
    | xa == xb  = (xa+xa) : groupEquals xs
    | otherwise = xa : groupEquals (xb : xs)

fillWithZeros :: [Int] -> [Int]
fillWithZeros l = l ++ replicate (4 - length l) 0

makeMove :: [Board] -> [Board]
makeMove matrix = result
    where removed = map (filter (/=0)) matrix
          result  = map (fillWithZeros . groupEquals) removed

rotl   = transpose . map reverse
rotr   = map reverse . transpose

move :: Board -> Direction -> Board
move board dir
    | dir == "LEFT"  = concat $ makeMove matrix
    | dir == "RIGHT" = concat $ mirror $ makeMove $ mirror matrix
    | dir == "UP"    = concat $ rotr $ makeMove $ rotl matrix
    | dir == "DOWN"  = concat $ rotl $ makeMove $ rotr matrix
        where mirror = map reverse
              matrix = chunks 4 board

prettyPrint :: Board -> IO ()
prettyPrint b = putStrLn $ intercalate "\n" $ map (intercalate "\t") (chunks 4 boardStr)
    where boardStr = map show b

freeIndexes :: Board -> [Int]
freeIndexes board = map snd freeTuples
    where indexes = zipWith (\x i->(x,i)) board [0..]
          freeTuples = filter (\(x,i)-> x==0) indexes

cpuMove :: IORef Board -> IO ()
cpuMove iorefBoard = do
    concreteBoard <- (readIORef iorefBoard)

    let frees = freeIndexes concreteBoard
    idx <- randomRIO (0, (length frees)-1) :: IO Int

    newCell <- randomRIO (1, 2) :: IO Int
    let newCell' = newCell * 2
    
    putStrLn "index frees:"
    print frees
    putStrLn "chosen index"
    print $ frees !! idx
    putStrLn "new val"
    print newCell'

    writeIORef iorefBoard (take (frees !! idx) concreteBoard ++ [newCell'] ++ drop ((frees !! idx)+1) concreteBoard)

    return ()
