import Test.HUnit 

import Test.Chess.Engine (testEngine)
import Test.Chess.Game   (testGame)
import Test.Chess.Input  (testInput)
import Test.Chess.Moves  (testMoves)

tests = TestList $ testGame
                ++ testEngine
                ++ testInput
                ++ testMoves

main = runTestTT tests
