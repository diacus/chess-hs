module Chess.Moves.Rook where

import Chess.Input
import Chess.Game
import Chess.Moves.Types

isValidRookMove :: MoveValidator
isValidRookMove gameStatus input
  | sourceFile == targetFile = undefined
  | targetRank == targetRank = undefined
  | otherwise = pushError gameStatus InvalidMove
  where sourceFile = (getFile . getSource) input
        targetFile = (getFile . getTarget) input

reachableRookCoordinates :: PathComputerCallback
reachableRookCoordinates (m, n) =
    let deltas = [-7 .. -1] ++ [1 .. 7]
     in [(m,n+d) | d <- deltas] ++ [(m+d,n) | d <- deltas]

