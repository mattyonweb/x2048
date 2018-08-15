 {-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.IORef
import System.IO
import System.Random
import Control.Monad
import X2048
import AI
 
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    b <- newIORef (fromString stringExample)   
    -- mainLoop b True
    mainLoopAi b

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

mainLoopAi :: IORef Board -> IO ()
mainLoopAi board = do
    readIORef board >>= prettyPrint
    putStrLn "================="

    concreteBoard <- readIORef board
    let chosenDirection = choice concreteBoard 
    writeIORef board (move concreteBoard chosenDirection)

    b' <- readIORef board
    
    if   (concreteBoard == b')
    then return ()
    else do
        putStrLn $ "Choosen dir: " ++ chosenDirection
        cpuMove board
        mainLoopAi board
