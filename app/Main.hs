import Data.List
import Data.IORef
import System.IO
import System.Random
import Control.Monad
import X2048

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    b <- newIORef (fromString stringExample)   
    mainLoop b True

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
