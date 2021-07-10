module Test.Chess.Game (testGame) where

import Data.Maybe (isNothing, fromJust)
import Test.HUnit

import Chess.Pieces
import Chess.Game

assertIsNothing msg value = assertBool msg (isNothing value)

testPosOfUniqPiece =
  TestCase (assertEqual "test getPositionOfUniqPiece [x]"
                        (Just ('a', 1))
                        (getPositionOfUniqPiece [(('a', 1), Piece None White)]))

testPosOfUniqPieceEmpty =
  TestCase (assertIsNothing "test getPositionOfUniqPiece x:xs"
                            (getPositionOfUniqPiece
                                [(('a', 1), Piece None White),
                                 (('b', 1), Piece None White)]))

testGame = [TestLabel "getPositionOfUniqPiece [x]" testPosOfUniqPiece,
            TestLabel "getPositionOfUniqPiece []"  testPosOfUniqPieceEmpty]
