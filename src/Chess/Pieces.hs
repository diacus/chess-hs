module Chess.Pieces where

data PieceColor = White | Black | Nobody
    deriving (Read, Show, Enum, Eq, Ord)

data PieceValue = None | Reachable | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Read, Show, Enum, Eq, Ord)

data Piece = Piece {getValue :: PieceValue, getColor :: PieceColor}
   deriving (Read, Eq)

whiteKing      = Piece King      White
whiteQueen     = Piece Queen     White
whiteRook      = Piece Rook      White
whiteBishop    = Piece Bishop    White
whiteKnight    = Piece Knight    White
whitePawn      = Piece Pawn      White
blackKing      = Piece King      Black
blackQueen     = Piece Queen     Black
blackRook      = Piece Rook      Black
blackBishop    = Piece Bishop    Black
blackKnight    = Piece Knight    Black
blackPawn      = Piece Pawn      Black
whiteReachable = Piece Reachable White
blackReachable = Piece Reachable Black

instance Show Piece where
  show piece 
    | piece == whiteKing      = " ♔ "
    | piece == whiteQueen     = " ♕ "
    | piece == whiteRook      = " ♖ "
    | piece == whiteBishop    = " ♗ "
    | piece == whiteKnight    = " ♘ "
    | piece == whitePawn      = " ♙ "
    | piece == blackKing      = " ♚ "
    | piece == blackQueen     = " ♛ "
    | piece == blackRook      = " ♜ "
    | piece == blackBishop    = " ♝ "
    | piece == blackKnight    = " ♞ "
    | piece == blackPawn      = " ♟ "
    | piece == whiteReachable = " ◌ "
    | piece == blackReachable = " ⚉ "
    | otherwise               = "   "


nextColor :: PieceColor -> PieceColor
nextColor White  = Black
nextColor Black  = White
nextColor Nobody = Nobody


areSameColor :: Piece -> Piece -> Bool
areSameColor p1 p2 = getColor p1 == getColor p2


areSameValue :: Piece -> Piece -> Bool
areSameValue p1 p2 = getValue p1 == getValue p2
