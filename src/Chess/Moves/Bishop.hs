module Chess.Moves.Bishop  where

import Chess.Moves.Types

isValidBishopMove :: MoveValidator
isValidBishopMove = isTargetInScope reachableBishopCoordinates

reachableBishopCoordinates :: PathComputerCallback
reachableBishopCoordinates (m, n) =
    let deltas = [-7 .. -1] ++ [1 .. 7]
     in [(m+d,n+d) | d <- deltas] ++ [(m+d,n-d) | d <- deltas]
