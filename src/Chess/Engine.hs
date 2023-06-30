module Chess.Engine where

import Chess.Pieces
import Chess.Game
import Chess.Input

import Chess.Moves.Bishop


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
  | hasError validatedPath = validatedPath
  | otherwise              = checkCellIsAvailable validatedPath input
  where isPieceFound  = isPieceAtCell gameStatus piece origin
        validatedPath = validatePath gameStatus input
        piece         = getPiece  input
        origin        = getOrigin input
        target        = getTarget input
        pieceValue    = getValue  piece


validatePath :: GameStatus -> Input -> GameStatus
validatePath gameStatus input
  | pieceValue == Rook   = validateRookMove gameStatus input
  | pieceValue == Knight = undefined
  | pieceValue == Bishop = validateBishopMove gameStatus input
  | pieceValue == Queen  = validateQueenMove gameStatus input
  | pieceValue == King   = undefined
  | pieceValue == Pawn   = undefined
  | otherwise = pushError gameStatus WTF
  where pieceValue = (getValue . getPiece) input


checkCellIsAvailable :: GameStatus -> Input -> GameStatus
checkCellIsAvailable gameStatus input
  | getValue piece == None                 = gameStatus
  | getColor piece == getPlayer gameStatus = pushError gameStatus MoveBlocked
  | otherwise                              = gameStatus
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


validateBishopMove :: GameStatus -> Input -> GameStatus
validateBishopMove gameStatus input
  | isValidMove == True = gameStatus
  | otherwise           = pushError gameStatus InvalidMove
  where validatedGameStatus = isValidBishopMove gameStatus input
        isValidMove         = hasErrors validatedGameStatus


validateQueenMove :: GameStatus -> Input -> GameStatus
validateQueenMove gameStatus input
  | originRank == targetRank = validateRookMove   gameStatus input
  | originFile == targetFile = validateRookMove   gameStatus input
  | otherwise                = validateBishopMove gameStatus input
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
