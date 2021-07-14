module Test.Chess.Engine.Rook (rookTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

rookTests = [ TestLabel "Move to empty cell" moveToEmptyCell
            , TestLabel "Cell is taken"      targetIsTaken
            , TestLabel "Row is blocked"     rowIsBlocked
            , TestLabel "Column is blocked"  columnIsBlocked
            , TestLabel "Invalid rook move"  invalidMove
            ]

moveToEmptyCell = TestCase (assertEqual message expected actual) where
  rawInput   = "R1a8"
  expected   = GameStatus [(target, whiteRook)] Black []
  gameStatus = GameStatus [(origin, whiteRook)] White []
  actual     = applyInput gameStatus rawInput
  origin     = ('a', 1)
  target     = ('a', 8)
  message    = "This test should pass"


targetIsTaken = TestCase (assertEqual message expected actual) where
  rawInput   = "R1a8"
  expected   = GameStatus board White [WTF]
  gameStatus = GameStatus board White []
  board      = [(origin, whiteRook), (target, whiteKing)]
  actual     = applyInput gameStatus rawInput
  origin     = ('a', 1)
  target     = ('a', 8)
  message    = getErrorMessage rawInput WTF


rowIsBlocked = TestCase (assertEqual message expected actual) where
  rawInput   = "Rah4"
  expected   = GameStatus board White [MoveBlocked]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whiteRook), (blocker, whitePawn)]
  origin     = ('a', 4)
  blocker    = ('e', 4)
  message    = getErrorMessage rawInput MoveBlocked


columnIsBlocked = TestCase (assertEqual message expected actual) where
  rawInput   = "R1a7"
  expected   = GameStatus board White [MoveBlocked]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whiteRook), (blocker, whitePawn)]
  origin     = ('a', 1)
  blocker    = ('a', 4)
  message    = getErrorMessage rawInput MoveBlocked


invalidMove = TestCase (assertEqual message expected actual) where
  rawInput   = "Rae1"
  expected   = GameStatus board White [InvalidMove]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whiteRook), (blocker, whitePawn)]
  origin     = ('a', 4)
  blocker    = ('e', 1)
  message    = getErrorMessage rawInput InvalidMove


getErrorMessage :: [Char] -> ChessError -> [Char]
getErrorMessage input expectedError =
  "input '" ++ input ++ "' should raise '" ++ (show expectedError) ++ "' error"
