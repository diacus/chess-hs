module Chess.Types where

type BoardCell = (Char, Int)

data Player = White | Black
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

data Input = Input {piece :: Piece, origin :: BoardCell, target::BoardCell}
    deriving (Read, Show, Eq)

type Move = (Piece, BoardCell, BoardCell)

type Board = [(BoardCell, Piece)]

data GameStatus = GameStatus {currentBoard :: Board, currentPlayer :: Player}
