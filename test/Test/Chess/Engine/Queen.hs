module Test.Chess.Engine.Queen (queenTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

queenTests = [ TestLabel "Queen test 1" queenTestOne
             , TestLabel "Queen test 2" queenNotFound
             ]

queenTestOne = TestCase (assertEqual "Queen" True True)

queenNotFound =
  TestCase (assertEqual ("Queen not found\n" ++ message) expected actual) where
    rawInput   = "Qeh4"
    gameStatus = GameStatus [(('e', 2), whiteQueen)] White []
    expected   = GameStatus [(('e', 2), whiteQueen)] White [InvalidMove]
    actual     = applyInput gameStatus rawInput
    message    = testMessage gameStatus rawInput actual

testMessage gameStatus input output =
  "\n" ++ input ++ "\n" ++ (show gameStatus) ++ "\n---\n" ++ (show output)
