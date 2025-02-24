module Chess.Input where

import Data.Char (ord)

import Chess.Pieces
import Chess.Board
import Chess.Game

data Input = Input {piece :: Piece, origin :: BoardCell, target::BoardCell}
    deriving (Read, Show, Eq)

type ParsedInput = (Maybe Input, Maybe ChessError)


parseInput :: GameStatus -> [Char] -> ParsedInput

parseInput _             [] = (Nothing, Just EmptyInput)
parseInput _         (_:[]) = (Nothing, Just ShortInput)
parseInput _ (_:_:_:_:_:xs) = (Nothing, Just LongInput)

parseInput gameStatus input = validateInput piece source target where
    piece  = getPieceFromInput  gameStatus input
    source = getSourceFromInput gameStatus piece input
    target = getTargetFromInput input


validateInput :: (Maybe Piece) -> (Maybe BoardCell) -> (Maybe BoardCell) -> ParsedInput

validateInput Nothing _       _       = (Nothing, Just CouldNotParsePiece)
validateInput _       Nothing _       = (Nothing, Just CouldNotParseSource)
validateInput _       _       Nothing = (Nothing, Just CouldNotParseTarget)

validateInput (Just piece) (Just source) (Just target) =
    let input = Input piece source target in (Just input, Nothing)


getPieceFromInput :: GameStatus -> [Char] -> Maybe Piece
getPieceFromInput (GameStatus _ player _) (_:_:[]) = Just (Piece Pawn player)
getPieceFromInput (GameStatus _ player _) (p:_:_:_) = parsePiece p player
getPieceFromInput _ _ = Nothing


getSourceFromInput :: GameStatus -> (Maybe Piece) -> [Char] -> Maybe BoardCell

getSourceFromInput (GameStatus board _ _) (Just piece) (_:_:_:[]) =
    findPiece board piece

getSourceFromInput (GameStatus board _ _) (Just piece) (_:fileOrRank:_:_:[]) =
    findPieceAtFileOrRank board piece fileOrRank

getSourceFromInput (GameStatus board _ _) (Just piece) (file:_:[]) =
    findPieceAtFile board piece file

getSourceFromInput _ _ _ = Nothing


getTargetFromInput :: [Char] -> Maybe BoardCell
getTargetFromInput     (file:rank:[]) = parseBoardCell file rank
getTargetFromInput   (_:file:rank:[]) = parseBoardCell file rank
getTargetFromInput (_:_:file:rank:[]) = parseBoardCell file rank
getTargetFromInput                  _ = Nothing


parseBoardCell :: Char -> Char -> Maybe BoardCell
parseBoardCell file rank = if elem file "abcdefgh" && elem rank "12345678"
                              then Just (file, parseRank rank)
                              else Nothing


parsePiece :: Char -> Player -> Maybe Piece
parsePiece p player =
    case p of
        'K' -> Just (Piece King   player)
        'Q' -> Just (Piece Queen  player)
        'B' -> Just (Piece Bishop player)
        'N' -> Just (Piece Knight player)
        'R' -> Just (Piece Rook   player)
        'P' -> Just (Piece Pawn   player)
        _   -> Nothing
