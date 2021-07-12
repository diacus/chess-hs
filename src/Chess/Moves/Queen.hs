module Chess.Moves.Queen (isValidQueenMove, reachableBoard) where

import Chess.Moves.Types
import Chess.Moves.Bishop
import Chess.Moves.Rook

isValidQueenMove :: MoveValidator
isValidQueenMove = isTargetInScope reachableCoordinates

reachableCoordinates :: PathComputerCallback
reachableCoordinates coord =
    (reachableRookCoordinates coord) ++ (reachableBishopCoordinates coord)

reachableBoard coord = reachableRookCoordinates coord
