module Chess.Moves (isValidMove) where

import Chess.Pieces
import Chess.Input
import Chess.Game

import Chess.Moves.Types  (MoveValidator)
import Chess.Moves.King   (isValidKingMove)
import Chess.Moves.Queen  (isValidQueenMove)
import Chess.Moves.Bishop (isValidBishopMove)
import Chess.Moves.Rook   (isValidRookMove)
import Chess.Moves.Knight (isValidKnightMove)
import Chess.Moves.Pawn   (isValidPawnMove)

isValidMove :: MoveValidator
isValidMove gameStatus input =
    isPieceInPlace gameStatus input && isValidDestiny gameStatus input


isPieceInPlace :: MoveValidator
isPieceInPlace gameStatus (Input piece position _) =
  isPieceAtCell gameStatus piece position


isValidDestiny :: MoveValidator
isValidDestiny gameStatus input
  | (value . piece) input == King   = isValidKingMove   gameStatus input
  | (value . piece) input == Queen  = isValidQueenMove  gameStatus input
  | (value . piece) input == Rook   = isValidRookMove   gameStatus input
  | (value . piece) input == Bishop = isValidBishopMove gameStatus input
  | (value . piece) input == Knight = isValidKnightMove gameStatus input
  | (value . piece) input == Pawn   = isValidPawnMove   gameStatus input
  | otherwise = False
