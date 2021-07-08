module Test.Chess.Moves.King (testKingMoves) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Moves.King

testKingMoves = [TestLabel "King with empty board" testKingOnEmptyGame]

emptyGame = GameStatus [] White []

testKingOnEmptyGame =
  TestCase (assertEqual "King with empty board"
                        False
                        (isValidKingMove [] White ('e', 1) ('e', 2)))
