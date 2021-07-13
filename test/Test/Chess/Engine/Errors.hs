module Test.Chess.Engine.Errors (errorTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

errorTests = [ TestLabel "Error test 1" errorTestOne ]


errorTestOne = TestCase (assertEqual "Error test one" expected actual) where
  rawInput   = "Qeh4"
  gameStatus = GameStatus [(('e', 2), whiteQueen)] White []
  expected   = GameStatus [(('e', 2), whiteQueen)] White [InvalidMove]
  actual     = applyInput gameStatus rawInput
  message    = "Error not raised: " ++ show InvalidMove
