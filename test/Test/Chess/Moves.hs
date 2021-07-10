module Test.Chess.Moves (testMoves) where

import Test.HUnit

import Chess.Input
import Chess.Pieces
import Chess.Game
import Chess.Moves

testMoves =
  [ TestLabel "isValidMove White King" testKingNotFound
  , TestLabel "isValidMove White King" testKingValidMove
  , TestLabel "isValidMove White Queen" testQueenValidMove
  ]

testKingNotFound =
  TestCase (assertEqual ("King not found\n" ++ (show initialGameStatus))
                        False
                        (isValidMove initialGameStatus input))
   where input = Input (Piece King White) ('a', 1) ('a', 2)

testKingValidMove =
  TestCase (assertEqual ("King valid move\n" ++ (show game))
                        True
                        (isValidMove game input))
    where game  = GameStatus [(('a', 1), (Piece King White))] White []
          input = Input (Piece King White) ('a', 1) ('a', 2)


testQueenValidMove =
  TestCase (assertEqual ("Queen valid move\n" ++ (show game))
                        True
                        (isValidMove game input))
    where game  = GameStatus [(('a', 1), (Piece Queen White))] White []
          input = Input (Piece Queen White) ('a', 1) ('h', 8)
