module TestPiecePaths where

import Data.List (sort)
import Test.HUnit

import ChessTypes
import PiecePaths

assertSetEqual msg xs ys = assertEqual msg (sort xs) (sort ys)

testRookMovesFromA1 =
  TestCase (assertSetEqual "Test move from Ra8"
                        ([('a',n) | n <- [1 .. 7]] ++ [(m, 8) | m <- "bcdefgh"])
                        (pieceMoves (Piece Rook Black) ('a',8)))

testBishopMovesFromC8 =
  TestCase (assertEqual "Test move from Bc8"
                        [('a',6), ('b',7), ('d',7), ('e',6), ('f',5), ('g',4), ('h',3)]
                        (pieceMoves (Piece Bishop White) ('c',8)))

testBlackPawnMovesFromC7 =
  TestCase (assertEqual "Test black move from Pc7"
                        [('b',6), ('c',5), ('c',6), ('d',6)]
                        (pieceMoves (Piece Pawn Black) ('c',7)))

testWhitePawnMovesFromE2 =
  TestCase (assertEqual "Test white move from Pe2"
                        [('d',3), ('e',3), ('e',4), ('f',3)]
                        (pieceMoves (Piece Pawn White) ('e',2)))

testsPiecePaths = [TestLabel "Test moving a rook"         testRookMovesFromA1,
                   TestLabel "Test moving a white bishop" testBishopMovesFromC8,
                   TestLabel "Test moving a white pawn"   testWhitePawnMovesFromE2,
                   TestLabel "Test moving a black pawn"   testBlackPawnMovesFromC7]
