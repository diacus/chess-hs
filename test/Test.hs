module Main where
import Test.HUnit 

import Test.Chess.Board  (testBoard)
import Test.Chess.Engine (testEngine)
import Test.Chess.Input  (testInput)
import Test.Chess.Moves.King  (testKingMoves)

import qualified System.Exit as Exit


tests = TestList $ testBoard
                ++ testEngine
                ++ testInput
                ++ testKingMoves

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
