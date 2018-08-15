import X2048
import Test.HUnit

main :: IO ()
main = do
    runTestTT tests
    return ()
    


board1 :: Board
board1 = fromString "2222222222222222"

test1 = TestCase (assertEqual "left: " [4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0] (move board1 "LEFT"))


board2 :: Board
board2 = fromString "0004222402280028"

test2 = TestCase (assertEqual "UP"    [2,4,4,8,0,0,2,16,0,0,0,0,0,0,0,0] (move board2 "UP"))
test3 = TestCase (assertEqual "DOWN"  [0,0,0,0,0,0,0,0,0,0,2,8,2,4,4,16] (move board2 "DOWN"))
test4 = TestCase (assertEqual "LEFT"  [4,0,0,0,4,2,4,0,4,8,0,0,2,8,0,0] (move board2 "LEFT"))

board3 :: Board
board3 = [4,0,0,0,4,0,0,2,8,0,0,0,64,32,0,0]
test5 = TestCase (assertEqual "DOWN" [0,0,0,0,8,0,0,0,8,0,0,0,64,32,0,2] (move board3 "DOWN"))

board4 = [4,0,0,0,0,0,0,0,2,0,0,0,4,0,4,0]
test6 = TestCase (assertEqual "SEI" [0,0,0,0,4,0,0,0,2,0,0,0,4,0,4,0] (move board4 "DOWN"))

board5 = [8,4,32,2,64,32,2,8,32,16,8,0,16,8,2,0]
test7 = TestCase (assertEqual "Sette" [11,15] (freeIndexes board5))

tests = TestList [ TestLabel "test uno: " test1
                 , TestLabel "test due: " test2
                 , TestLabel "test due: " test3
                 , TestLabel "test due: " test4
                 , TestLabel "test due: " test5
                 , TestLabel "test due: " test6
                 , TestLabel "test due: " test7 ]
