module AI where

import X2048
import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)

-- | Generate every possible CPU moves that could follow the current board.
everyPossibleCpuMove :: Board -> [Board]
everyPossibleCpuMove b = withTwos ++ withFours
    where frees = freeIndexes b
          withTwos  = [b] >>= (\x -> [replace i 2 x | i <- frees])
          withFours = [b] >>= (\x -> [replace i 4 x | i <- frees])

-- | Generates the 4 translation of a board.
--
-- In other words, this function returns the four boards the user can get by moving up, down, left or right.
-- In case the movement in one of such directions is impossible (e.g. no tile can be moved to the left), an empty list will be returned.
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
    where usrChoices    = boardTranslations board :: [Board]
          trees         = map (mkCpuMoveTree 2) usrChoices :: [CpuMoveTree]
          evaluatedDirs = map cpuTreeEval trees :: [Float]
          indexOfBest   = fromJust $ elemIndex (maximum evaluatedDirs) evaluatedDirs

-- | Evaluates a board.
evaluate :: Board -> Float
evaluate board = fromIntegral (emptyBlocks * (monotonicity board)) * (variance board)
    where emptyBlocks = quot (length $ freeIndexes board) 2

-- | Gives a rate of monotonicity.
-- A monotonic row (or column) is made of tiles in order. For example:
-- [2,4,8,16] is a perfectly fine monotonic row;
-- [16,8,4,2] is also monotonic;
-- [4,256,2,0] is not monotonic;
-- [2,2,2,2] is monotonic.
-- The more monotonic a board, the better.
monotonicity :: Board -> Int
monotonicity board = maximum $ map monotony boards
    where boards = generateRotations board
          monotony m = sum $ map (\row -> if (row == (sort row)) then 1 else 0) m 

-- | Calculates the variance of a board.
-- Variances gives the idea of how much tiles of the same row/columns differs from one another. For example:
-- [2,2,2,2] will have the lowest variance
-- [2,4,8,16] will have a greater, but still low variance
-- [1024,1024,1024,1024] will have the lowest variance, again
-- [256,2,4,2] will have a big variance
-- The smaller a variance value, the better.
variance :: Board -> Float
variance board = (fromIntegral 4) / (fromIntegral (1 + var))
    where avg = sum board `quot` (length board)
          var = sum $ map (\c -> abs (avg - c)) board


data UserMoveTree = UserMoveTree Board [CpuMoveTree]
                  | UserMoveLeaf Board deriving (Show)
                  
data CpuMoveTree  = CpuMoveTree Board [UserMoveTree]
                  | CpuMoveLeaf Board deriving (Show)

mkCpuMoveTree :: Int -> Board -> CpuMoveTree
mkCpuMoveTree 0 b = CpuMoveLeaf b
mkCpuMoveTree n b = CpuMoveTree b $ map (mkUserMoveTree (n-1)) (boardTranslations b)

mkUserMoveTree :: Int -> Board -> UserMoveTree
mkUserMoveTree 0 b = UserMoveLeaf b
mkUserMoveTree n b = UserMoveTree b $ map (mkCpuMoveTree (n-1)) (everyPossibleCpuMove b)

userTreeEval :: UserMoveTree -> Float
userTreeEval (UserMoveLeaf b)   = evaluate b 
userTreeEval (UserMoveTree b l) = evaluate b + ((sum $ map cpuTreeEval l) / (fromIntegral $ (1 + length l)))
-- userTreeEval (UserMoveTree b l) = evaluate b + (median $ map cpuTreeEval l)

cpuTreeEval :: CpuMoveTree -> Float
cpuTreeEval (CpuMoveLeaf b)   = evaluate b 
cpuTreeEval (CpuMoveTree b l) = evaluate b + ((sum $ map userTreeEval l) / (fromIntegral $ (1 + length l)))
-- cpuTreeEval (CpuMoveTree b l) = evaluate b + (median $ map userTreeEval l)

