import Test.HUnit 

import Test.Chess.Board  (testBoard)
import Test.Chess.Engine (testEngine)
import Test.Chess.Input  (testInput)
import Test.Chess.Moves.King  (testKingMoves)

tests = TestList $ testBoard
                ++ testEngine
                ++ testInput
                ++ testKingMoves

main = runTestTT tests
