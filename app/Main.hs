import Utils 
import System.IO
import System.Environment (getArgs)
import X2048 (Board, move, cpuMove, prettyPrint, initialBoard)
import qualified AI (choice)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho      stdin False

    board <- initialBoard
    args  <- getArgs
    case args of
        [] -> mainLoopHumanGame True board
        
        ["ai"] -> do
             _ <- mainLoopAiGame board
             return ()
             
        ["ts"] -> do
             hSetEcho stdin True
             putStrLn "How many games?"
             numTests <- (read <$> getLine) :: IO Int
             hSetEcho stdin False
             runMultipleGames board [] numTests
             
        _ -> do
             putStrLn "One or more command line commands not recognized. Aborting"
             return ()


-- | Translates numeric pad input to directions (to improve significantly)
keystrokes :: Char -> IO Direction
keystrokes c = return dir
  where dir = case c of
                '8' -> Up
                '6' -> Utils.Right
                '4' -> Utils.Left
                '2' -> Down
                _   -> NoDir


-- | Asks the user for a direction until it gets a correct one.
getInputDirection :: IO Direction
getInputDirection = do
  c <- getChar >>= keystrokes
  case c of
    NoDir -> getInputDirection
    otherDir -> return otherDir 


-- | Loop for a turn in a game with a human (human move and cpu move)
mainLoopHumanGame :: Bool -> Board -> IO ()
mainLoopHumanGame doPrint board = do
    if doPrint
        then do
            prettyPrint board
            putStrLn "================="
        else return ()

    dir <- getInputDirection
    let nextBoard = move board dir
    
    if   (board == nextBoard)
    then mainLoopHumanGame False nextBoard
    else (cpuMove nextBoard) >>= (mainLoopHumanGame True)


-- | Loop for a turn in a game with CPU only (move decision of the CPU and
-- the CPU answer)
mainLoopAiGame :: Board -> IO Board
mainLoopAiGame board = do    
    let chosenDirection = AI.choice board 
    let newBoard = move board chosenDirection

    if   (board == newBoard)
    then return newBoard
    else cpuMove newBoard >>= mainLoopAiGame


-- | TODO: remove
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


-- | Run multiple AI games.
runMultipleGames :: Board -> [Int] -> Int -> IO ()
runMultipleGames board maximums iterations = do
    if (length maximums < iterations)
    then do
        endBoard <- mainLoopAiGame board
        prettyPrint endBoard
        putStrLn $ show $ maximum endBoard
        newBoard <- initialBoard
        runMultipleGames newBoard (maximum endBoard : maximums) iterations
    else do
        putStrLn $ "128: " ++ (show $ count 128 maximums)
        putStrLn $ "256: " ++ (show $ count 256 maximums)
        putStrLn $ "512: " ++ (show $ count 512 maximums)
        putStrLn $ "1024: " ++ (show $ count 1024 maximums)
        putStrLn $ "2048: " ++ (show $ count 2048 maximums)
