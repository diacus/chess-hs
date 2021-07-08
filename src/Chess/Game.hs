module Chess.Game where

import Chess.Pieces
import Chess.Board

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
    GameStatus {getBoard  :: Board, getPlayer :: Player, getErrors :: ErrorStack}
               deriving (Read, Eq)

instance Show GameStatus where
   show (GameStatus board _ [])    = showBoard board
   show (GameStatus board _ (e:_)) = (showBoard board) ++ "\nError: " ++ (show e)


hasError :: GameStatus -> Bool
hasError (GameStatus _ _ []) = False
hasError (GameStatus _ _ (e:_)) = True


pushError :: GameStatus -> ChessError -> GameStatus
pushError (GameStatus board player errors) e = GameStatus board player (e:errors)


popError :: GameStatus -> (Maybe ChessError, GameStatus)
popError (GameStatus b p []) = (Nothing, (GameStatus b p []))
popError (GameStatus b p (e:es)) = (Just e, GameStatus b p es)


initialGameStatus :: GameStatus
initialGameStatus = GameStatus initialBoard White []
