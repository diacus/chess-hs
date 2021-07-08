module Test.Chess.Input (testInput) where

import Data.Maybe (isNothing, fromJust)
import Test.HUnit

import Chess.Pieces
import Chess.Input
import Chess.Game


assertMaybeMoveEqual msg expexted output = assertEqual msg expexted (fromJust output)
assertIsNothing msg value = assertBool msg (isNothing value)


testInputIsTooLarge =
  TestCase (assertEqual "parseInput \"Kde3Q\""
                        (Nothing, Just LongInput)
                        (parseInput initialGameStatus "Kde3Q"))

testInputIsEmpty =
  TestCase (assertEqual "parseInput \"\""
                        (Nothing, Just EmptyInput)
                        (parseInput initialGameStatus ""))

testParseMoveN1d6 =
  TestCase (assertEqual "parseInput \"N1d6\"" (Nothing, Just CouldNotParseSource) (parseInput initialGameStatus "N1d6"))

testParseMoveKed3 =
  TestCase (assertEqual "parseInput \"Ked3\""
                        (Just (Input (Piece King White) ('e',1) ('d',3)), Nothing)
                        (parseInput initialGameStatus "Ked3"))

testParseMoveQe3 =
  TestCase (assertEqual "parseInput \"Qe3\""
                        (Just (Input (Piece Queen White) ('d',1) ('e',3)), Nothing)
                        (parseInput initialGameStatus "Qe3"))

testParseMoveNbd6 =
  TestCase (assertEqual "parseInput \"Nbd6\""
                        (Just (Input (Piece Knight White) ('b',1) ('d',6)), Nothing)
                        (parseInput initialGameStatus "Nbd6"))

testParseMoveC4 =
  TestCase (assertEqual "parseInput \"c4\""
                        (Just (Input (Piece Pawn White) ('c', 2) ('c', 4)), Nothing)
                        (parseInput initialGameStatus "c4"))


testInput = [TestLabel "parseInput Kde2Q" testInputIsTooLarge,
             TestLabel "parseInput ''"    testInputIsEmpty,
             TestLabel "parseInput Kde3"  testParseMoveKed3,
             TestLabel "parseInput Qe3"   testParseMoveQe3,
             TestLabel "parseInput c4"    testParseMoveC4,
             TestLabel "parseInput K1d6"  testParseMoveN1d6,
             TestLabel "parseInput Kbd6"  testParseMoveNbd6]
