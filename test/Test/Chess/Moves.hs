module Test.Chess.Moves (testMoves) where

import Test.HUnit

import Chess.Input
import Chess.Pieces
import Chess.Game
import Chess.Moves

testMoves =
  [ TestLabel "isValidMove White King"  testKingNotFound
  , TestLabel "isValidMove White King"  testKingValidMove
  , TestLabel "isValidMove White Queen" testQueenValidMove
  , TestLabel "isValidMove White Queen" testQueenInalidMove
  , TestLabel "isValidMove White Queen" testQueenCannotJumpPiece
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

testQueenInalidMove =
  TestCase (assertEqual ("Queen valid move\n" ++ (show game))
                        False
                        (isValidMove game input))
    where game  = GameStatus [(('a', 1), (Piece Queen White))] White []
          input = Input (Piece Queen White) ('a', 1) ('h', 7)

testQueenCannotJumpPiece =
  TestCase (assertEqual ("Queen valid move\nInput: " ++ (show input)
                                                     ++ "\nBoard:\n"
                                                     ++ (show game))
                        False
                        (isValidMove game input))
    where game  = GameStatus [ (('a', 1), (Piece Queen White))
                             , (('g', 7), (Piece Pawn Black))]
                             White
                             []
          input = Input (Piece Queen White) ('a', 1) ('h', 8)

