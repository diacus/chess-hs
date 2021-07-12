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
isPieceInPlace gameStatus input =
  isPieceAtCell gameStatus (getPiece input) (getOrigin input)


isValidDestiny :: MoveValidator
isValidDestiny gameStatus input
  | value == King   = isValidKingMove   gameStatus input
  | value == Queen  = isValidQueenMove  gameStatus input
  | value == Rook   = isValidRookMove   gameStatus input
  | value == Bishop = isValidBishopMove gameStatus input
  | value == Knight = isValidKnightMove gameStatus input
  | value == Pawn   = isValidPawnMove   gameStatus input
  | otherwise = False
  where value = (getValue . getPiece) input
