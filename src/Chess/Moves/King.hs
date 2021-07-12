module Chess.Moves.King (isValidKingMove) where

import Chess.Pieces
import Chess.Input
import Chess.Game
import Chess.Moves.Types


isValidKingMove :: MoveValidator
isValidKingMove gameStatus input =
    isOneStepAway input && isCellEmpty gameStatus (getTarget input)


isOneStepAway input = elem target targets where
  origin  = (cellToCoordinate . getOrigin) input
  target  = (cellToCoordinate . getTarget) input
  targets = reachableCoordinates origin


reachableCoordinates :: PathComputerCallback
reachableCoordinates (m, n) =
  let deltas = [-1, 0, 1]
   in [(m + i, n + j) | i <- deltas, j <- deltas, (0,0) /= (i,j)]
