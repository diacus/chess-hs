module Chess.Moves.Pawn (isValidPawnMove, validatePawnMove) where

import Data.Char (ord)

import Chess.Moves.Types (MoveValidator)
import Chess.Game
import Chess.Input
import Chess.Pieces


-- | Validate a pawn move for use in Engine.hs
-- Returns GameStatus unchanged if valid, or with InvalidMove error if invalid
validatePawnMove :: MoveValidator
validatePawnMove gameStatus input
    | isValidForward      = gameStatus
    | isValidDoublePush   = gameStatus
    | isValidCapture      = gameStatus
    | otherwise           = pushError gameStatus InvalidMove
    where
        board              = getBoard gameStatus
        piece              = getPiece input
        player             = getColor piece
        origin             = getOrigin input
        target             = getTarget input
        originFile         = getFile origin
        originRank         = getRank origin
        targetFile         = getFile target
        targetRank         = getRank target

        -- Direction: White moves up (rank increases), Black moves down (rank decreases)
        direction          = if player == White then 1 else -1
        startRank          = if player == White then 2 else 7

        -- Forward one square
        isForwardMove      = targetFile == originFile
                             && targetRank == originRank + direction

        -- Forward two squares from starting position
        isDoublePush       = originRank == startRank
                             && targetFile == originFile
                             && targetRank == originRank + 2 * direction

        -- Diagonal capture (one square forward-diagonally)
        fileDelta          = ord targetFile - ord originFile
        isDiagonalCapture  = abs fileDelta == 1
                             && targetRank == originRank + direction

        -- Get piece at target
        targetPiece        = getPieceAt board target
        targetIsOpponent   = getColor targetPiece == nextColor player

        -- Target square must be empty for forward moves
        isValidForward     = isForwardMove && isCellEmpty gameStatus target

        -- Both intermediate and target squares must be empty for double push
        intermediateCell   = (originFile, originRank + direction)
        isValidDoublePush  = isDoublePush
                             && isCellEmpty gameStatus intermediateCell
                             && isCellEmpty gameStatus target

        -- Target must have opponent's piece for capture
        isValidCapture     = isDiagonalCapture && targetIsOpponent


isValidPawnMove :: MoveValidator
isValidPawnMove = undefined
