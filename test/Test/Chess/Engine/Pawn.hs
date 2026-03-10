module Test.Chess.Engine.Pawn (pawnTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

pawnTests = [ TestLabel "White pawn forward one square"   whitePawnForward
            , TestLabel "White pawn double push"           whitePawnDoublePush
            , TestLabel "White pawn diagonal capture"     whitePawnCapture
            , TestLabel "White pawn blocked forward"      whitePawnBlocked
            , TestLabel "White pawn blocked double push"  whitePawnDoublePushBlocked
            , TestLabel "White pawn invalid move"          whitePawnInvalidMove
            , TestLabel "White pawn capture empty cell"   whitePawnCaptureEmpty
            , TestLabel "Black pawn forward one square"    blackPawnForward
            , TestLabel "Black pawn double push"           blackPawnDoublePush
            , TestLabel "Black pawn diagonal capture"     blackPawnCapture
            , TestLabel "Black pawn blocked forward"      blackPawnBlocked
            , TestLabel "Black pawn blocked double push"   blackPawnDoublePushBlocked
            , TestLabel "Black pawn invalid move"          blackPawnInvalidMove
            , TestLabel "Black pawn capture empty cell"    blackPawnCaptureEmpty
            ]


-- White pawn tests

whitePawnForward = TestCase (assertEqual message expected actual) where
  rawInput   = "e3"
  expected   = GameStatus [(target, whitePawn)] Black []
  gameStatus = GameStatus [(origin, whitePawn)] White []
  actual     = applyInput gameStatus rawInput
  origin     = ('e', 2)
  target     = ('e', 3)
  message    = "White pawn should move forward one square"


whitePawnDoublePush = TestCase (assertEqual message expected actual) where
  rawInput   = "e4"
  expected   = GameStatus [(target, whitePawn)] Black []
  gameStatus = GameStatus [(origin, whitePawn)] White []
  actual     = applyInput gameStatus rawInput
  origin     = ('e', 2)
  target     = ('e', 4)
  message    = "White pawn should be able to double push from starting position"


whitePawnCapture = TestCase (assertEqual message expected actual) where
  rawInput   = "exd3"
  expected   = GameStatus [(target, whitePawn)] Black []
  gameStatus = GameStatus [(origin, whitePawn), (target, blackPawn)] White []
  actual     = applyInput gameStatus rawInput
  origin     = ('e', 2)
  target     = ('d', 3)
  message    = "White pawn should capture diagonally"


whitePawnBlocked = TestCase (assertEqual message expected actual) where
  rawInput   = "e3"
  expected   = GameStatus board White [MoveBlocked]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whitePawn), (target, blackPawn)]
  origin     = ('e', 2)
  target     = ('e', 3)
  message    = getErrorMessage MoveBlocked rawInput


whitePawnDoublePushBlocked = TestCase (assertEqual message expected actual) where
  rawInput   = "e4"
  expected   = GameStatus board White [MoveBlocked]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whitePawn), (intermediate, blackPawn)]
  origin     = ('e', 2)
  intermediate = ('e', 3)
  message    = getErrorMessage MoveBlocked rawInput


whitePawnInvalidMove = TestCase (assertEqual message expected actual) where
  rawInput   = "e5"
  expected   = GameStatus board White [InvalidMove]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whitePawn)]
  origin     = ('e', 2)
  message    = getErrorMessage InvalidMove rawInput


whitePawnCaptureEmpty = TestCase (assertEqual message expected actual) where
  rawInput   = "exd3"
  expected   = GameStatus board White [InvalidMove]
  gameStatus = GameStatus board White []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, whitePawn)]
  origin     = ('e', 2)
  message    = "Pawn capture requires an opponent piece on target"


-- Black pawn tests

blackPawnForward = TestCase (assertEqual message expected actual) where
  rawInput   = "e6"
  expected   = GameStatus [(target, blackPawn)] White []
  gameStatus = GameStatus [(origin, blackPawn)] Black []
  actual     = applyInput gameStatus rawInput
  origin     = ('e', 7)
  target     = ('e', 6)
  message    = "Black pawn should move forward one square"


blackPawnDoublePush = TestCase (assertEqual message expected actual) where
  rawInput   = "e5"
  expected   = GameStatus [(target, blackPawn)] White []
  gameStatus = GameStatus [(origin, blackPawn)] Black []
  actual     = applyInput gameStatus rawInput
  origin     = ('e', 7)
  target     = ('e', 5)
  message    = "Black pawn should be able to double push from starting position"


blackPawnCapture = TestCase (assertEqual message expected actual) where
  rawInput   = "exd6"
  expected   = GameStatus [(target, blackPawn)] White []
  gameStatus = GameStatus [(origin, blackPawn), (target, whitePawn)] Black []
  actual     = applyInput gameStatus rawInput
  origin     = ('e', 7)
  target     = ('d', 6)
  message    = "Black pawn should capture diagonally"


blackPawnBlocked = TestCase (assertEqual message expected actual) where
  rawInput   = "e6"
  expected   = GameStatus board Black [MoveBlocked]
  gameStatus = GameStatus board Black []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, blackPawn), (target, whitePawn)]
  origin     = ('e', 7)
  target     = ('e', 6)
  message    = getErrorMessage MoveBlocked rawInput


blackPawnDoublePushBlocked = TestCase (assertEqual message expected actual) where
  rawInput   = "e5"
  expected   = GameStatus board Black [MoveBlocked]
  gameStatus = GameStatus board Black []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, blackPawn), (intermediate, whitePawn)]
  origin     = ('e', 7)
  intermediate = ('e', 6)
  message    = getErrorMessage MoveBlocked rawInput


blackPawnInvalidMove = TestCase (assertEqual message expected actual) where
  rawInput   = "e4"
  expected   = GameStatus board Black [InvalidMove]
  gameStatus = GameStatus board Black []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, blackPawn)]
  origin     = ('e', 7)
  message    = getErrorMessage InvalidMove rawInput


blackPawnCaptureEmpty = TestCase (assertEqual message expected actual) where
  rawInput   = "exd6"
  expected   = GameStatus board Black [InvalidMove]
  gameStatus = GameStatus board Black []
  actual     = applyInput gameStatus rawInput
  board      = [(origin, blackPawn)]
  origin     = ('e', 7)
  message    = "Pawn capture requires an opponent piece on target"


-- Helper function
getErrorMessage :: ChessError -> [Char] -> [Char]
getErrorMessage expectedError input =
  "input '" ++ input ++ "' should raise '" ++ (show expectedError) ++ "' error"