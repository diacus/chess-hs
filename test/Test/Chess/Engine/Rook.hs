module Test.Chess.Engine.Rook (rookTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

rookTests = [ TestLabel "Rook cannot jump fellow pieces" jumpPieceSameColor
            , TestLabel "Invalid rook move should fail"  moveNotAllowed
            ]

jumpPieceSameColor = TestCase (assertEqual message expected actual) where
  rawInput   = "Rah4"
  expected   = GameStatus board White [MoveBlocked]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whiteRook), (blocker, whitePawn)]
  origin     = ('a', 4)
  blocker    = ('e', 4)
  message    = getErrorMessage rawInput MoveBlocked


moveNotAllowed = TestCase (assertEqual message expected actual) where
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
