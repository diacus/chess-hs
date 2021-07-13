module Test.Chess.Engine.Queen (queenTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

queenTests = [ TestLabel "Queen test 1" queenTestOne ]

queenTestOne = TestCase (assertEqual "Queen" True True)

testMessage gameStatus input output =
  "\n" ++ input ++ "\n" ++ (show gameStatus) ++ "\n---\n" ++ (show output)
