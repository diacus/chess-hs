module Chess.Moves.Queen (isValidQueenMove) where

import Chess.Moves.Types
import Chess.Moves.Bishop
import Chess.Moves.Rook

isValidQueenMove :: MoveValidator
isValidQueenMove = isTargetInScope reachableCoordinates

reachableCoordinates :: PathComputerCallback
reachableCoordinates coord =
    (reachableRookCoordinates coord) ++ (reachableBishopCoordinates coord)
