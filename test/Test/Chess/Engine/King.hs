module Test.Chess.Engine.King (kingTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine
import Chess.Input

kingTests = [ TestLabel "Move one square up"             moveOneSquareUp
            , TestLabel "Move one square down"           moveOneSquareDown
            , TestLabel "Move one square left"           moveOneSquareLeft
            , TestLabel "Move one square right"          moveOneSquareRight
            , TestLabel "Move one square diagonal NE"   moveDiagonalNE
            , TestLabel "Move one square diagonal NW"   moveDiagonalNW
            , TestLabel "Move one square diagonal SE"   moveDiagonalSE
            , TestLabel "Move one square diagonal SW"   moveDiagonalSW
            , TestLabel "Capture opponent piece"         captureOpponent
            , TestLabel "Blocked by own piece"           blockedByOwnPiece
            , TestLabel "Invalid move too far"           invalidMoveTooFar
            , TestLabel "Invalid knight move"            invalidKnightMove
            ]

-- Helper to check if move was successful (no errors)
assertSuccess :: String -> GameStatus -> Assertion
assertSuccess msg gs = assertBool msg (null (getErrors gs))

-- Helper to check if move produced expected error
assertError :: String -> ChessError -> GameStatus -> Assertion
assertError msg err gs = assertBool msg (err `elem` getErrors gs)

moveOneSquareUp :: Test
moveOneSquareUp = TestCase $ do
  let rawInput   = "Ke1e2"
      gameStatus = GameStatus [(('e', 1), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square up" actual

moveOneSquareDown :: Test
moveOneSquareDown = TestCase $ do
  let rawInput   = "Ke2e1"
      gameStatus = GameStatus [(('e', 2), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square down" actual

moveOneSquareLeft :: Test
moveOneSquareLeft = TestCase $ do
  let rawInput   = "Ked1"
      gameStatus = GameStatus [(('e', 1), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square left" actual

moveOneSquareRight :: Test
moveOneSquareRight = TestCase $ do
  let rawInput   = "Kef1"
      gameStatus = GameStatus [(('e', 1), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square right" actual

moveDiagonalNE :: Test
moveDiagonalNE = TestCase $ do
  let rawInput   = "Kc1d2"
      gameStatus = GameStatus [(('c', 1), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square diagonal NE" actual

moveDiagonalNW :: Test
moveDiagonalNW = TestCase $ do
  let rawInput   = "Kc3b2"
      gameStatus = GameStatus [(('c', 3), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square diagonal NW" actual

moveDiagonalSE :: Test
moveDiagonalSE = TestCase $ do
  let rawInput   = "Kc3d4"
      gameStatus = GameStatus [(('c', 3), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square diagonal SE" actual

moveDiagonalSW :: Test
moveDiagonalSW = TestCase $ do
  let rawInput   = "Kc3b4"
      gameStatus = GameStatus [(('c', 3), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should move one square diagonal SW" actual

captureOpponent :: Test
captureOpponent = TestCase $ do
  let rawInput   = "Ke1d2"
      gameStatus = GameStatus [(('e', 1), whiteKing), (('d', 2), blackQueen)] White []
      actual     = applyInput gameStatus rawInput
  assertSuccess "King should capture opponent piece" actual

blockedByOwnPiece :: Test
blockedByOwnPiece = TestCase $ do
  let rawInput   = "Ke1e2"
      gameStatus = GameStatus [(('e', 1), whiteKing), (('e', 2), whitePawn)] White []
      actual     = applyInput gameStatus rawInput
  assertError "King cannot move to square occupied by own piece" MoveBlocked actual

invalidMoveTooFar :: Test
invalidMoveTooFar = TestCase $ do
  let rawInput   = "Ke1e4"
      gameStatus = GameStatus [(('e', 1), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertError "King cannot move more than one square" InvalidMove actual

invalidKnightMove :: Test
invalidKnightMove = TestCase $ do
  let rawInput   = "Ke1f3"
      gameStatus = GameStatus [(('e', 1), whiteKing)] White []
      actual     = applyInput gameStatus rawInput
  assertError "King cannot move like a knight" InvalidMove actual
