module AI where

import X2048
import Data.Maybe
import Data.List

m = fromString stringExample

replace :: Int -> a -> [a] -> [a]
replace idx newCell list = take idx list ++ [newCell] ++ drop (idx+1) list

everyPossibleBoard :: Board -> [Board]
everyPossibleBoard b = withTwos ++ withFours
    where frees = freeIndexes b
          withTwos  = [b] >>= (\x -> [replace i 2 x | i <- frees])
          withFours = [b] >>= (\x -> [replace i 4 x | i <- frees])

evaluate :: Board -> Int
evaluate board = emptyBlocks
    where emptyBlocks = length $ freeIndexes board
          
userMoves :: Board -> [Board]
userMoves board = map (\b -> if b == board then [] else b) boards  -- Evita di rimanere fermo
    where boards = map (move board) ["UP", "DOWN", "LEFT", "RIGHT"]

convertToDir :: Int -> Direction
convertToDir idx = case idx of
                    0 -> "UP"
                    1 -> "DOWN"
                    2 -> "LEFT"
                    3 -> "RIGHT"
                    
-- choice :: Board -> Direction
choice board = convertToDir $ fromJust $ elemIndex (maximum choiceValues) choiceValues
    where possibilities = map everyPossibleBoard $ userMoves board :: [[Board]]
          choiceValues  = map (sum . map evaluate) possibilities
