module AI where

import X2048
import Data.Maybe
import Data.List

m = fromString stringExample

-- | Replaces element at IDX with newCell.
replace :: Int -> a -> [a] -> [a]
replace idx newCell list = take idx list ++ [newCell] ++ drop (idx+1) list

-- | Generate every possible CPU moves that could follow the current board.
everyPossibleCpuMove :: Board -> [Board]
everyPossibleCpuMove b = withTwos ++ withFours
    where frees = freeIndexes b
          withTwos  = [b] >>= (\x -> [replace i 2 x | i <- frees])
          withFours = [b] >>= (\x -> [replace i 4 x | i <- frees])

-- | Evaluates a board.
evaluate :: Board -> Int
evaluate board = emptyBlocks * (monotonicity board)
    where emptyBlocks = length $ freeIndexes board

-- | Generates the 4 translation of a board.
boardTranslations :: Board -> [Board]
boardTranslations board = map (\b -> if b == board then [] else b) boards  -- Evita di rimanere fermo
    where boards = map (move board) ["UP", "DOWN", "LEFT", "RIGHT"]

-- | __(ugly)__ Converts from idx to direction. Strictly related to the
-- function boardTranslations.
convertToDir :: Int -> Direction
convertToDir idx = case idx of
                    0 -> "UP"
                    1 -> "DOWN"
                    2 -> "LEFT"
                    3 -> "RIGHT"

-- | Given a board, return the best direction where to move the board according
-- to the AI.
choice :: Board -> Direction
choice board = convertToDir indexOfBest
    where possibilities = map everyPossibleCpuMove $ boardTranslations board
          -- evaluatedDirs = map (sum . map evaluate) possibilities
          firstLevelEval b = evaluate b 
          evaluatedDirs = map (\board -> evaluate board + (quot (sum $ map evaluate (everyPossibleCpuMove board)) (length possibilities))) (boardTranslations board)
          indexOfBest   = fromJust $ elemIndex (maximum evaluatedDirs) evaluatedDirs


-- f l = map (\board -> evaluate board + (`quot` (map (sum . map evaluate board) possibilities) (length possibilites))) boardTranslations board
-- | Gives a rate of monotonicity.
monotonicity :: Board -> Int
monotonicity board = maximum $ map monotony boards
    where boards = generateRotations board
          monotony m = sum $ map (\row -> if (row == (sort row)) then 1 else 0) m 
