module Chess where

import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)

type BoardCell = (Int, Int)

data Player = White | Black
    deriving (Read, Show, Enum, Eq, Ord)

data PieceValue = Pawn | Knight | Bishop | Rook | Queen | King | None
    deriving (Read, Show, Enum, Eq, Ord)


data Piece = Piece {value :: PieceValue, player :: Player}
   deriving (Read, Eq)

instance Show Piece where
    show (Piece value color) = case (Piece value color) of
       Piece King   White -> " ♔"
       Piece Queen  White -> " ♕"
       Piece Rook   White -> " ♖"
       Piece Bishop White -> " ♗"
       Piece Knight White -> " ♘"
       Piece Pawn   White -> " ♙"
       Piece King   Black -> " ♚"
       Piece Queen  Black -> " ♛"
       Piece Rook   Black -> " ♜"
       Piece Bishop Black -> " ♝"
       Piece Knight Black -> " ♞"
       Piece Pawn   Black -> " ♟"
       Piece None   _     -> "  "


type Board = [(BoardCell, Piece)]

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White

getPieceAt :: Board -> BoardCell -> Piece
getPieceAt board cell = let thePiece = lookup cell board
                            in fromMaybe (Piece None Black) thePiece

boardToRanks :: Board -> [(Int, [Piece])]
boardToRanks board =
    [(rank, [getPieceAt board (file,rank) | file <- [0..7]]) | rank <- [0..7]]

rankToStr :: (Int, [Piece]) -> [Char]
rankToStr (i, ps) = '\n': (label ++ row ++ "  " ++ label) where
    label = (show.succ) i
    row = concat $ (map show) ps

showBoard :: Board -> [Char]
showBoard board = filesLabel ++ stringRanks ++ filesLabel where
    stringRanks = (concat . (map rankToStr) . boardToRanks) board
    filesLabel  = "\n  a b c d e f g h"

getPosOf :: Board -> Piece -> BoardCell
getPosOf board piece = fst cell where
    cell = head [(pos, piece) | (pos, piece) <- board]


movePiece :: Board -> Piece -> BoardCell -> BoardCell -> Board
movePiece board piece fromCell toCell = (toCell, piece):boardWithoutPiece where
    boardWithoutPiece =
        [(cell', piece') | (cell', piece') <- board,
                           (cell', piece') /= (fromCell, piece)]

initialBoard :: Board
initialBoard = [((0, 7), (Piece Rook   Black)),
                ((1, 7), (Piece Knight Black)),
                ((2, 7), (Piece Bishop Black)),
                ((3, 7), (Piece Queen  Black)),
                ((4, 7), (Piece King   Black)),
                ((5, 7), (Piece Bishop Black)),
                ((6, 7), (Piece Knight Black)),
                ((7, 7), (Piece Rook   Black)),
                ((0, 6), (Piece Pawn   Black)),
                ((1, 6), (Piece Pawn   Black)),
                ((2, 6), (Piece Pawn   Black)),
                ((3, 6), (Piece Pawn   Black)),
                ((4, 6), (Piece Pawn   Black)),
                ((5, 6), (Piece Pawn   Black)),
                ((6, 6), (Piece Pawn   Black)),
                ((7, 6), (Piece Pawn   Black)),

                ((0, 0), (Piece Rook   White)),
                ((1, 0), (Piece Knight White)),
                ((2, 0), (Piece Bishop White)),
                ((3, 0), (Piece Queen  White)),
                ((4, 0), (Piece King   White)),
                ((5, 0), (Piece Bishop White)),
                ((6, 0), (Piece Knight White)),
                ((7, 0), (Piece Rook   White)),
                ((0, 1), (Piece Pawn   White)),
                ((1, 1), (Piece Pawn   White)),
                ((2, 1), (Piece Pawn   White)),
                ((3, 1), (Piece Pawn   White)),
                ((4, 1), (Piece Pawn   White)),
                ((5, 1), (Piece Pawn   White)),
                ((6, 1), (Piece Pawn   White)),
                ((7, 1), (Piece Pawn   White))]

play :: IO ()
play = showMeTheBoard newBoard where
    showMeTheBoard = putStrLn . showBoard
    newBoard       = movePiece initialBoard piece from to where
        piece = Piece Rook White
        from  = (0, 0)
        to    = (4, 4)
