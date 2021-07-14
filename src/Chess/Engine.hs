module Chess.Engine where

import Chess.Pieces
import Chess.Game
import Chess.Input


applyInput :: GameStatus -> [Char] -> GameStatus
applyInput gameStatus rawInput = apply status input where
  (_, status) = popError gameStatus
  input       = parseInput status rawInput


apply :: GameStatus -> ParsedInput -> GameStatus
apply gameStatus (Nothing, Just e) = pushError gameStatus e
apply gameStatus (Just input, Nothing)
  | hasError validatedStatus = validatedStatus
  | otherwise                = movePiece validatedStatus input
  where validatedStatus = validate gameStatus input


movePiece :: GameStatus -> Input -> GameStatus
movePiece (GameStatus board player errors) (Input piece from to) =
  if elem (from, piece) board
     then GameStatus newBoard (nextColor player) errors
     else GameStatus board player (PieceNotFound:errors)
          where newBoard = (to, piece):boardWithoutPiece
                boardWithoutPiece = filter (/=(from,piece)) board


validate :: GameStatus -> Input -> GameStatus
validate gameStatus input
  | origin       == target = pushError gameStatus WTF
  | isPieceFound == False  = pushError gameStatus PieceNotFound
  | isCellFree   == False  = pushError gameStatus MoveBlocked
  | pieceValue   == Rook   = validateRookMove gameStatus input
  | otherwise              = undefined
  where isPieceFound = isPieceAtCell gameStatus piece origin
        isCellFree   = checkCellIsAvailable gameStatus input
        piece        = getPiece  input
        origin       = getOrigin input
        target       = getTarget input
        pieceValue   = getValue  piece


checkCellIsAvailable :: GameStatus -> Input -> Bool
checkCellIsAvailable gameStatus input
  | getValue piece == None                 = True
  | getColor piece == getPlayer gameStatus = False
  | otherwise                              = True
  where piece = getPieceAt (getBoard gameStatus) (getTarget input)


validateRookMove :: GameStatus -> Input -> GameStatus
validateRookMove gameStatus input
  | originRank == targetRank = checkPathIsFree gameStatus rankPath
  | originFile == targetFile = checkPathIsFree gameStatus filePath
  | otherwise                = pushError gameStatus InvalidMove
  where originFile = (getFile . getOrigin) input
        targetFile = (getFile . getTarget) input
        originRank = (getRank . getOrigin) input
        targetRank = (getRank . getTarget) input
        rankPath   = getRankPath originRank originFile targetFile
        filePath   = getFilePath originFile originRank targetRank


checkPathIsFree :: GameStatus -> [BoardCell] -> GameStatus
checkPathIsFree gameStatus [] = gameStatus
checkPathIsFree gameStatus xs =
  if all (isCellEmpty gameStatus) xs
     then gameStatus
     else pushError gameStatus MoveBlocked
