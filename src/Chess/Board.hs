module Chess.Board where

import Data.Char (ord, isDigit)
import Data.Maybe (fromJust, fromMaybe, isNothing)

import Chess.Pieces
import Chess.TextUI

type BoardCell = (Char, Int)

type Board = [(BoardCell, Piece)]

getPieceAt :: Board -> BoardCell -> Piece
getPieceAt board cell = let thePiece = lookup cell board
                         in fromMaybe (Piece None Black) thePiece


boardToRanks :: Board -> [[Piece]]
boardToRanks board =
    [[getPieceAt board (file,rank) | file <- ['a' .. 'h']] | rank <- [1..8]]


rankToStr :: (Int, [Piece]) -> [Char]
rankToStr (rank, ps) = '\n': (label ++ row ++ "  " ++ label) where
    label = show rank
    row = concat $ (map show) ps


showBoard :: Board -> [Char]
showBoard = linesToText . mergePicesAndBorders . boardToRanks


findPieceAtFileOrRank :: Board -> Piece -> Char -> Maybe BoardCell
findPieceAtFileOrRank board piece from =
    if isDigit from
       then findPieceAtRank board piece (parseRank from)
       else findPieceAtFile board piece from


parseRank :: Char -> Int
parseRank r = ord r - ord '0'


findPieceAtRank :: Board -> Piece -> Int -> Maybe BoardCell
findPieceAtRank board piece rank =
    let candidates = filter (\cell -> snd cell == piece && (snd.fst) cell == rank) board
     in getPositionOfUniqPiece candidates


findPieceAtFile :: Board -> Piece -> Char -> Maybe BoardCell
findPieceAtFile board piece file =
  let candidates = filter (\cell -> snd cell == piece && (fst.fst) cell == file) board
   in getPositionOfUniqPiece candidates


findPiece :: Board -> Piece -> Maybe BoardCell
findPiece board piece =
  let cs = filter (\c -> snd c == piece) board
   in getPositionOfUniqPiece cs


getPositionOfUniqPiece :: Board -> Maybe BoardCell
getPositionOfUniqPiece [(cell, _)] = Just cell
getPositionOfUniqPiece _ = Nothing


initialBoard :: Board
initialBoard = [(('a', 8), (Piece Rook   Black)),
                (('b', 8), (Piece Knight Black)),
                (('c', 8), (Piece Bishop Black)),
                (('d', 8), (Piece Queen  Black)),
                (('e', 8), (Piece King   Black)),
                (('f', 8), (Piece Bishop Black)),
                (('g', 8), (Piece Knight Black)),
                (('h', 8), (Piece Rook   Black)),
                (('a', 7), (Piece Pawn   Black)),
                (('b', 7), (Piece Pawn   Black)),
                (('c', 7), (Piece Pawn   Black)),
                (('d', 7), (Piece Pawn   Black)),
                (('e', 7), (Piece Pawn   Black)),
                (('f', 7), (Piece Pawn   Black)),
                (('g', 7), (Piece Pawn   Black)),
                (('h', 7), (Piece Pawn   Black)),

                (('a', 1), (Piece Rook   White)),
                (('b', 1), (Piece Knight White)),
                (('c', 1), (Piece Bishop White)),
                (('d', 1), (Piece Queen  White)),
                (('e', 1), (Piece King   White)),
                (('f', 1), (Piece Bishop White)),
                (('g', 1), (Piece Knight White)),
                (('h', 1), (Piece Rook   White)),
                (('a', 2), (Piece Pawn   White)),
                (('b', 2), (Piece Pawn   White)),
                (('c', 2), (Piece Pawn   White)),
                (('d', 2), (Piece Pawn   White)),
                (('e', 2), (Piece Pawn   White)),
                (('f', 2), (Piece Pawn   White)),
                (('g', 2), (Piece Pawn   White)),
                (('h', 2), (Piece Pawn   White))]

