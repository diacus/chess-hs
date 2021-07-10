module Chess.Moves.King (isValidKingMove) where

import Chess.Pieces
import Chess.Input
import Chess.Game
import Chess.Moves.Types

isValidKingMove :: MoveValidator
isValidKingMove  = isTargetInScope reachableCoordinates

reachableCoordinates :: PathComputerCallback
reachableCoordinates (m, n) =
    let deltas = [-1, 0, 1]
     in [(m + i, n + j) | i <- deltas, j <- deltas, (0,0) /= (i,j)]
