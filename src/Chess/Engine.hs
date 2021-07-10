module Chess.Engine where

import Chess.Pieces
import Chess.Game
import Chess.Input
import Chess.Moves


applyInput :: GameStatus -> [Char] -> GameStatus
applyInput gameStatus rawInput = apply status input where
    (_, status) = popError gameStatus
    input  = parseInput status rawInput


apply :: GameStatus -> ParsedInput -> GameStatus
apply gameStatus (Nothing, Just e) = pushError gameStatus e
apply gameStatus (Just input, Nothing) =
    if isValidMove gameStatus input
       then movePiece gameStatus input
       else pushError gameStatus InvalidMove


movePiece :: GameStatus -> Input -> GameStatus
movePiece (GameStatus board player errors) (Input piece from to) =
    if elem (from, piece) board
       then GameStatus newBoard (nextPlayer player) errors
       else GameStatus board player (PieceNotFound:errors)
            where
                newBoard = (to, piece):boardWithoutPiece
                boardWithoutPiece = filter (/=(from,piece)) board
