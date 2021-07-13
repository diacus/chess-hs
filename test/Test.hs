import Test.HUnit 

import Test.Chess.Engine (engineTests)
import Test.Chess.Input  (testInput)

tests = TestList $ engineTests
                ++ testInput

main = runTestTT tests
