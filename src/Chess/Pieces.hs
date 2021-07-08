module Chess.Pieces where

data Player = White | Black | Nobody
    deriving (Read, Show, Enum, Eq, Ord)

data PieceValue = None | Reachable | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Read, Show, Enum, Eq, Ord)

data Piece = Piece {value :: PieceValue, player :: Player}
   deriving (Read, Eq)

instance Show Piece where
    show (Piece value player) = case (Piece value player) of
       Piece King      White -> " ♔"
       Piece Queen     White -> " ♕"
       Piece Rook      White -> " ♖"
       Piece Bishop    White -> " ♗"
       Piece Knight    White -> " ♘"
       Piece Pawn      White -> " ♙"
       Piece King      Black -> " ♚"
       Piece Queen     Black -> " ♛"
       Piece Rook      Black -> " ♜"
       Piece Bishop    Black -> " ♝"
       Piece Knight    Black -> " ♞"
       Piece Pawn      Black -> " ♟"
       Piece Reachable White -> " ◌"
       Piece Reachable Black -> " ⚉"
       Piece None      _     -> "  "

nextPlayer :: Player -> Player
nextPlayer White  = Black
nextPlayer Black  = White
nextPlayer Nobody = Nobody
