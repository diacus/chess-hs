module Chess.Moves (isValidMove) where

import Chess.Pieces
import Chess.Board
import Chess.Game
import Chess.Input
import Chess.Moves.King
import Chess.Moves.Queen
import Chess.Moves.Bishop
import Chess.Moves.Rook
import Chess.Moves.Knight
import Chess.Moves.Pawn

isValidMove :: GameStatus -> Input -> Bool
isValidMove _ _ = True
-- isValidMove (GameStatus board player _) (Input piece from to) =
--     isPieceInPlace board player piece from && isValidDestiny board piece from to


isPieceInPlace :: Board -> Player -> Piece -> BoardCell -> Bool
isPieceInPlace = undefined


isValidDestiny :: Board -> Piece -> BoardCell -> BoardCell -> Bool
isValidDestiny board piece from to
    | value piece == King   = isValidKingMove   board (player piece) from to
    | value piece == Queen  = isValidQueenMove  board (player piece) from to
    | value piece == Rook   = isValidRookMove   board (player piece) from to
    | value piece == Bishop = isValidBishopMove board (player piece) from to
    | value piece == Knight = isValidKnightMove board (player piece) from to
    | value piece == Pawn   = isValidPawnMove   board (player piece) from to
    | otherwise             = False
