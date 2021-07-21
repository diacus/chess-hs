module Test.Chess.Engine.Queen (queenTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

queenTests = [ TestLabel "Move to the left"       left
             , TestLabel "Move to the right"      right
             , TestLabel "Move to the bottom"     bottom
             , TestLabel "Move to the up"         up
             , TestLabel "Move to the left up"    leftUp
             , TestLabel "Move to the left down"  leftDown
             , TestLabel "Move to the right up"   rightUp
             , TestLabel "Move to the right down" rightDown
             ]

left = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Qae2"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('a', 2)
  target     = ('e', 2)
  message    = "This test should pass"

right = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Qea2"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('e', 2)
  target     = ('a', 2)
  message    = "This test should pass"

bottom = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Q1d8"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('d', 1)
  target     = ('d', 8)
  message    = "This test should pass"

up = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Q8d1"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('d', 8)
  target     = ('d', 1)
  message    = "This test should pass"

leftUp = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Q8h1"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('a', 8)
  target     = ('h', 1)
  message    = "This test should pass"

leftDown = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Q1h8"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('a', 1)
  target     = ('h', 8)
  message    = "This test should pass"

rightUp = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Q8a1"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('h', 8)
  target     = ('a', 1)
  message    = "This test should pass"

rightDown = TestCase (assertEqual message expected actual) where
  actual     = applyInput gameStatus rawInput
  rawInput   = "Q8a1"
  expected   = GameStatus [(target, whiteQueen)] Black []
  gameStatus = GameStatus [(origin, whiteQueen)] White []
  origin     = ('h', 8)
  target     = ('a', 1)
  message    = "This test should pass"

testMessage gameStatus input output =
  "\n" ++ input ++ "\n" ++ (show gameStatus) ++ "\n---\n" ++ (show output)
