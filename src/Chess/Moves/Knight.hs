module Chess.Moves.Knight (isValidKnightMove) where

import Chess.Moves.Types

isValidKnightMove :: MoveValidator
isValidKnightMove = isTargetInScope reachableCoordinates

reachableCoordinates :: PathComputerCallback
reachableCoordinates (m, n) =
    let kJumps = [-2, -1, 1, 2]
     in [(m + i, n + j) | i <- kJumps, j <- kJumps, abs i /= abs j]

