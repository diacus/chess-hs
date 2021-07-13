module Chess.Game where

import Data.Char (ord, isDigit)
import Data.Maybe (fromJust, fromMaybe, isNothing)

import Chess.Pieces
import Chess.TextUI

type BoardCell = (Char, Int)
type Board = [(BoardCell, Piece)]


data ChessError = AmbiguousPiece
                | BadInput
                | InvalidMove
                | MoveBlocked
                | PieceNotFound
                | CouldNotParsePiece
                | CouldNotParseSource
                | CouldNotParseTarget
                | EmptyInput
                | ShortInput
                | LongInput
                | WTF
                deriving (Read, Enum, Eq, Ord)

instance Show ChessError where
  show AmbiguousPiece      = "The imput refers to more than one piece or none"
  show BadInput            = "Invalid input, verify the chess notation"
  show InvalidMove         = "The move is invalid for the piece"
  show MoveBlocked         = "There is a piece in the path that block the move" 
  show PieceNotFound       = "Selected piece is not in the choosen location"
  show CouldNotParsePiece  = "Could not parse the specified piece"
  show CouldNotParseSource = "Could not parse the specified source cell"
  show CouldNotParseTarget = "Could not parse the specified target cell"
  show EmptyInput          = "The input is empty"
  show ShortInput          = "The input is too short"
  show LongInput           = "The input is too long"
  show WTF                 = "Khe ^e2g@s!"


type ErrorStack = [ChessError]


data GameStatus =
    GameStatus {getBoard  :: Board, getPlayer :: PieceColor, getErrors :: ErrorStack}
               deriving (Read, Eq)


instance Show GameStatus where
   show (GameStatus board _ [])    = showBoard board
   show (GameStatus board _ (e:_)) = (showBoard board) ++ "\nError: " ++ (show e)


--      a   b   c   d   e   f   g   h
--    ┌───┬───┬───┬───┬───┬───┬───┬───┐
--  1 │ ♖ │ ♘ │ ♗ │ ♕ │ ♔ │ ♗ │ ♘ │ ♖ │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  2 │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  3 │   │   │   │   │   │   │   │   │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  4 │   │   │   │   │   │   │   │   │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  5 │   │   │   │   │   │   │   │   │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  6 │   │   │   │   │   │   │   │   │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  7 │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │
--    ├───┼───┼───┼───┼───┼───┼───┼───┤
--  8 │ ♜ │ ♞ │ ♝ │ ♛ │ ♚ │ ♝ │ ♞ │ ♜ │
--    └───┴───┴───┴───┴───┴───┴───┴───┘
initialBoard :: Board
initialBoard = [(('a', 1), (whiteRook)),
                (('b', 1), (whiteKnight)),
                (('c', 1), (whiteBishop)),
                (('d', 1), (whiteQueen)),
                (('e', 1), (whiteKing)),
                (('f', 1), (whiteBishop)),
                (('g', 1), (whiteKnight)),
                (('h', 1), (whiteRook)),
                (('a', 2), (whitePawn)),
                (('b', 2), (whitePawn)),
                (('c', 2), (whitePawn)),
                (('d', 2), (whitePawn)),
                (('e', 2), (whitePawn)),
                (('f', 2), (whitePawn)),
                (('g', 2), (whitePawn)),
                (('h', 2), (whitePawn)),

                (('a', 8), (blackRook)),
                (('b', 8), (blackKnight)),
                (('c', 8), (blackBishop)),
                (('d', 8), (blackQueen)),
                (('e', 8), (blackKing)),
                (('f', 8), (blackBishop)),
                (('g', 8), (blackKnight)),
                (('h', 8), (blackRook)),
                (('a', 7), (blackPawn)),
                (('b', 7), (blackPawn)),
                (('c', 7), (blackPawn)),
                (('d', 7), (blackPawn)),
                (('e', 7), (blackPawn)),
                (('f', 7), (blackPawn)),
                (('g', 7), (blackPawn)),
                (('h', 7), (blackPawn))]


initialGameStatus :: GameStatus
initialGameStatus = GameStatus initialBoard White []


isPieceAtCell :: GameStatus -> Piece -> BoardCell -> Bool
isPieceAtCell (GameStatus board _ _) piece position
  | getPieceAt board position == piece = True
  | otherwise                          = False


isCellEmpty gameStatus cell
  | isNothing search = True
  | otherwise        = False
  where search = lookup cell (getBoard gameStatus)


hasError :: GameStatus -> Bool
hasError (GameStatus _ _ []) = False
hasError (GameStatus _ _ (e:_)) = True


pushError :: GameStatus -> ChessError -> GameStatus
pushError (GameStatus board player errors) e = GameStatus board player (e:errors)


popError :: GameStatus -> (Maybe ChessError, GameStatus)
popError (GameStatus b p []) = (Nothing, (GameStatus b p []))
popError (GameStatus b p (e:es)) = (Just e, GameStatus b p es)

showBoard :: Board -> [Char]
showBoard = linesToText . mergePicesAndBorders . boardToRanks


boardToRanks :: Board -> [[Piece]]
boardToRanks board =
    [[getPieceAt board (file,rank) | file <- ['a' .. 'h']] | rank <- [1..8]]


getPieceAt :: Board -> BoardCell -> Piece
getPieceAt board cell = let thePiece = lookup cell board
                         in fromMaybe (Piece None Black) thePiece


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
