module Chess.Input (parseInput) where

import Data.Char (isDigit, chr, ord)
import Data.Maybe (fromJust, isNothing)

import Chess.Types

parseRank :: Char -> Int
parseRank r = ord r - ord '0'

prevFile :: Char -> Char
prevFile = chr . (\c -> c - 1) . ord

nextFile :: Char -> Char
nextFile = succ

parsePieceValue :: Char -> PieceValue
parsePieceValue input =
    case input of
        'K' -> King
        'Q' -> Queen
        'B' -> Bishop
        'N' -> Knight
        'R' -> Rook
        'P' -> Pawn
        _   -> None

parseCell :: Char -> Char -> Maybe BoardCell
parseCell file rank = if (elem file "abcdefg") && isDigit rank
                         then Just (file, parseRank rank)
                         else Nothing

findPieceAtRank :: Board -> Piece -> Int -> Maybe BoardCell
findPieceAtRank board piece rank = if length candidates == 1 then Just ((fst.head) candidates) else Nothing
    where
        candidates = filter (\cell -> snd cell == piece && (snd.fst) cell == rank) board

findPieceAtFile :: Board -> Piece -> Char -> Maybe BoardCell
findPieceAtFile board piece file = if length candidates == 1 then Just ((fst.head) candidates) else Nothing
    where
        candidates = filter (\cell -> snd cell == piece && (fst.fst) cell == file) board

findPiece :: Board -> Piece -> Maybe BoardCell
findPiece board piece = if length candidates == 1 then Just ((fst.head) candidates) else Nothing
    where
        candidates = filter (\cell -> snd cell == piece) board


parseInput :: GameStatus -> [Char] -> Maybe Input
parseInput gameStatus (p:from:toFile:toRank:[]) =
    if isNothing cell then Nothing else Just (Input piece fromCell toCell) where
         piece    = Piece (parsePieceValue p) (currentPlayer gameStatus)
         board    = currentBoard gameStatus
         toCell   = (toFile, parseRank toRank)
         cell     = if isDigit from
                        then findPieceAtRank board piece (parseRank from)
                        else findPieceAtFile board piece from
         fromCell = fromJust cell

parseInput gameStatus (p:file:toRank:[]) =
    if isNothing position then Nothing else Just (Input piece fromCell toCell) where
        board    = currentBoard gameStatus
        piece    = Piece (parsePieceValue p) (currentPlayer gameStatus)
        position = findPiece board piece
        fromCell = fromJust position
        toCell   = (file, parseRank toRank)

parseInput gameStatus (toFile:toRank:[]) =
    if isNothing position then Nothing else Just (Input piece fromCell toCell) where
        board = currentBoard gameStatus
        piece = Piece Pawn (currentPlayer gameStatus)
        position = findPieceAtFile board piece toFile
        fromCell = fromJust position
        toCell   = (toFile, (parseRank toRank))

parseInput _ _ = Nothing
