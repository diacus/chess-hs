module Chess.Moves.Rook where

import Chess.Moves.Types

isValidRookMove :: MoveValidator
isValidRookMove = isTargetInScope reachableRookCoordinates

reachableRookCoordinates :: PathComputerCallback
reachableRookCoordinates (m, n) =
    let deltas = [-7 .. -1] ++ [1 .. 7]
     in [(m,n+d) | d <- deltas] ++ [(m+d,n) | d <- deltas]

