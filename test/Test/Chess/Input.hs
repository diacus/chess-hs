module Test.Chess.Input (testInput) where

import Data.Maybe (isNothing, fromJust)
import Test.HUnit

import Chess.Types
import Chess.Input
import Chess (initialBoard)

assertMaybeMoveEqual msg expexted output = assertEqual msg expexted (fromJust output)
assertIsNothing msg value = assertBool msg (isNothing value)

testStatus = GameStatus initialBoard White

testInputIsTooLarge =
  TestCase (assertIsNothing "parseInput \"Kde3Q\"" (parseInput testStatus "Kde3Q"))

testInputIsEmpty =
  TestCase (assertIsNothing "parseInput \"\"" (parseInput testStatus ""))

testParseMoveN1d6 =
  TestCase (assertIsNothing "parseInput \"N1d6\"" (parseInput testStatus "N1d6"))

testParseMoveKed3 =
  TestCase (assertMaybeMoveEqual "parseInput \"Ked3\""
                                 (Input (Piece King White) ('e',1) ('d',3))
                                 (parseInput testStatus "Ked3"))

testParseMoveQe3 =
  TestCase (assertMaybeMoveEqual "parseInput \"Qe3\""
                                 (Input (Piece Queen White) ('d',1) ('e',3))
                                 (parseInput testStatus "Qe3"))

testParseMoveNbd6 =
  TestCase (assertMaybeMoveEqual "parseInput \"Nbd6\""
                                 (Input (Piece Knight White) ('b',1) ('d',6))
                                 (parseInput testStatus "Nbd6"))

testParseMoveC4 =
  TestCase (assertMaybeMoveEqual "parseInput \"c4\""
                                 (Input (Piece Pawn White) ('c', 2) ('c', 4))
                                 (parseInput testStatus "c4"))

testInput = [TestLabel "parseInput Kde2Q" testInputIsTooLarge,
             TestLabel "parseInput ''"    testInputIsEmpty,
             TestLabel "parseInput Kde3"  testParseMoveKed3,
             TestLabel "parseInput Qe3"   testParseMoveQe3,
             TestLabel "parseInput c4"    testParseMoveC4,
             TestLabel "parseInput K1d6"  testParseMoveN1d6,
             TestLabel "parseInput Kbd6"  testParseMoveNbd6]
