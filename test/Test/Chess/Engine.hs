module Test.Chess.Engine (testEngine) where

import Data.Maybe (isNothing, fromJust)
import Test.HUnit

import Chess.Pieces
import Chess.Engine
import Chess.Game
import Chess.Input

assertMaybeMoveEqual msg expexted output = assertEqual msg expexted (fromJust output)
assertIsNothing msg value = assertBool msg (isNothing value)

whiteQueen = Piece Queen White
whiteKing  = Piece King White
queenStarts = ('a', 1)
queenEnds   = ('c', 6)
input = Input whiteQueen queenStarts queenEnds
startStatus = GameStatus [(queenStarts, whiteQueen)] White []
endStatus = GameStatus [(queenEnds, whiteQueen)] Black []

testMovePiece =
  TestCase (assertEqual "testMovePiece"
                        (GameStatus [(queenEnds, whiteQueen)] Black [])
                        (movePiece (GameStatus [(queenStarts, whiteQueen)] White [])
                                   (Input whiteQueen queenStarts queenEnds)))


testPieceNotFound =
  TestCase (assertEqual "testMovePiece unexisting piece"
                        (GameStatus [(('a', 1), whiteKing)] White [PieceNotFound])
                        (movePiece (GameStatus [(('a', 1), whiteKing)] White [])
                                   (Input whiteQueen ('b', 1) ('b', 2))))


testEngine = [TestLabel "movePiece" testMovePiece,
              TestLabel "movePiece piece not found" testPieceNotFound]
