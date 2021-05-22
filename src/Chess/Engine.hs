module Chess.Engine where

import Data.Char (chr, isDigit, isLower, ord)
import Data.Maybe (fromJust, fromMaybe, isNothing)

import Chess.Types
import Chess.Input (parseInput)

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White

getFile :: BoardCell -> Char
getFile (file, _) = file

getRank :: BoardCell -> Int
getRank (_, rank) = rank

getPieceAt :: Board -> BoardCell -> Piece
getPieceAt board cell = let thePiece = lookup cell board
                            in fromMaybe (Piece None Black) thePiece

boardToRanks :: Board -> [(Int, [Piece])]
boardToRanks board =
    [(rank, [getPieceAt board (file,rank) | file <- ['a' .. 'h']]) | rank <- [1..8]]

rankToStr :: (Int, [Piece]) -> [Char]
rankToStr (rank, ps) = '\n': (label ++ row ++ "  " ++ label) where
    label = show rank
    row = concat $ (map show) ps

showBoard :: Board -> [Char]
showBoard board = filesLabel ++ stringRanks ++ filesLabel where
    stringRanks = (concat . (map rankToStr) . boardToRanks) board
    filesLabel  = "\n  a b c d e f g h"

showGameStatus :: GameStatus -> [Char]
showGameStatus gameStatus = "It's " ++ ((show.currentPlayer) gameStatus) ++ " move" ++ ((showBoard.currentBoard) gameStatus)

getPosOf :: Board -> Piece -> BoardCell
getPosOf board piece = fst cell where
    cell = head [(pos, piece) | (pos, piece) <- board]

movePiece :: GameStatus -> Input -> GameStatus
movePiece gameStatus input = GameStatus newBoard player where
    player = (nextPlayer.currentPlayer) gameStatus
    newBoard = ((target input), (piece input)):boardWithoutPiece
    boardWithoutPiece =
        [(cell', piece') | (cell', piece') <- (currentBoard gameStatus),
                           (cell', piece') /= ((origin input), (piece input))]

meansPawn :: [Char] -> Bool
meansPawn = isLower . head

isCapture :: [Char] -> Bool
isCapture = elem 'x'

parseDestinyCell :: [Char] -> BoardCell
parseDestinyCell input = let (file, rank) = lastTwo in (file, (ord rank - ord '0')) where
                             lastTwo = ((\(r:f:xs) -> (f, r)) . reverse) input

nextPawnRank :: Player -> Bool -> Int -> Int
nextPawnRank White movesTwo rank = if movesTwo then rank + 2 else rank + 1
nextPawnRank Black movesTwo rank = if movesTwo then rank - 2 else rank - 1

isPieceAtFile :: Char -> (BoardCell, Piece) -> Bool
isPieceAtFile file cell = file == (getFile.fst) cell

cellBelongsTo :: Player -> (BoardCell, Piece) -> Bool
cellBelongsTo thePlayer (_, piece) = thePlayer == player piece

isValidMove :: Board -> Move -> Bool
isValidMove _ _ = True  -- TODO: Implement when a move is valid


applyInput :: GameStatus -> [Char] -> GameStatus
applyInput gameStatus rawInput = if isNothing parsedInput then gameStatus else newGameStatus where
    board = currentBoard gameStatus
    player = currentPlayer gameStatus
    parsedInput = parseInput gameStatus rawInput
    input = fromJust parsedInput
    newGameStatus = movePiece gameStatus input
