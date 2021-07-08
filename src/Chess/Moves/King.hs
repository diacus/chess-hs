module Chess.Moves.King where

import Chess.Pieces
import Chess.Board
import Chess.Game
import Chess.Coordinate

isValidKingMove :: Board -> Player -> BoardCell -> BoardCell -> Bool
isValidKingMove board player source target =
    let reachable = Piece Reachable player
     in elem (target, reachable) (reachableCells board player source)

reachableCells :: Board -> Player -> BoardCell -> Board
reachableCells = undefined

reachableCoordinates :: PathComputerCallback
reachableCoordinates (m, n) =
    let deltas = [-1, 0, 1]
     in [(m + i, n + j) | i <- deltas, j <- deltas, (0,0) /= (i,j)]
