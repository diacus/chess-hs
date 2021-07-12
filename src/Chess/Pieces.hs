module Chess.Pieces where

data PieceColor = White | Black | Nobody
    deriving (Read, Show, Enum, Eq, Ord)

data PieceValue = None | Reachable | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Read, Show, Enum, Eq, Ord)

data Piece = Piece {getValue :: PieceValue, getColor :: PieceColor}
   deriving (Read, Eq)

instance Show Piece where
    show piece = case piece of
       Piece King      White -> " ♔ "
       Piece Queen     White -> " ♕ "
       Piece Rook      White -> " ♖ "
       Piece Bishop    White -> " ♗ "
       Piece Knight    White -> " ♘ "
       Piece Pawn      White -> " ♙ "
       Piece King      Black -> " ♚ "
       Piece Queen     Black -> " ♛ "
       Piece Rook      Black -> " ♜ "
       Piece Bishop    Black -> " ♝ "
       Piece Knight    Black -> " ♞ "
       Piece Pawn      Black -> " ♟ "
       Piece Reachable White -> " ◌ "
       Piece Reachable Black -> " ⚉ "
       Piece None      _     -> "   "

nextColor :: PieceColor -> PieceColor
nextColor White  = Black
nextColor Black  = White
nextColor Nobody = Nobody
