import Test.HUnit 
import Test.Chess.Input (testInput)

tests = TestList testInput
main = runTestTT tests
