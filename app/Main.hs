 {-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.IORef
import System.IO
import System.Random
import Control.Monad
import System.Environment (getArgs)
import X2048
import AI
 
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    b <- newIORef (fromString stringExample)
    
    args <- getArgs
    case args of
        [] -> mainLoop b True
        ["ai"] -> do
                    mainLoopAi b
                    return ()
        ["ts"] -> counter b [] 100

mainLoop :: IORef Board -> Bool -> IO ()
mainLoop board printOrNot = do
    if printOrNot
        then do
            readIORef board >>= prettyPrint
            putStrLn "================="
        else return ()

    c <- getChar
    
    concreteBoard <- readIORef board
    case c of
        '8' -> writeIORef board (move concreteBoard "UP")
        '6' -> writeIORef board (move concreteBoard "RIGHT")
        '4' -> writeIORef board (move concreteBoard "LEFT")
        '2' -> writeIORef board (move concreteBoard "DOWN")
        _   -> mainLoop board False

    b' <- readIORef board
    
    if (concreteBoard == b')
        then mainLoop board False
        else do
            cpuMove board
            mainLoop board True

mainLoopAi :: IORef Board -> IO Board
mainLoopAi board = do    
    concreteBoard <- readIORef board
    
    let chosenDirection = choice concreteBoard 
    writeIORef board (move concreteBoard chosenDirection)

    b' <- readIORef board

    if   (concreteBoard == b')
    then return b'
    else do
        cpuMove board
        mainLoopAi board

count :: Eq a => [a] -> a -> Int
count [] n = 0
count (x:xs) n = if (x==n) then 1 + count xs n else count xs n
 
counter board maximums iterations = do
    if (length maximums < iterations)
    then do
        endBoard <- mainLoopAi board
        prettyPrint endBoard
        putStrLn $ show $ maximum endBoard
        writeIORef board (fromString stringExample)
        counter board (maximum endBoard : maximums) iterations
    else do
        putStrLn $ "128: " ++ (show $ count maximums 128)
        putStrLn $ "256: " ++ (show $ count maximums 256)
        putStrLn $ "512: " ++ (show $ count maximums 512)
        putStrLn $ "1024: " ++ (show $ count maximums 1024)
        putStrLn $ "2048: " ++ (show $ count maximums 2048)
