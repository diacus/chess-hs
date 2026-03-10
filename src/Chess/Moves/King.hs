module Chess.Moves.King (isValidKingMove, reachableKingCoordinates) where

import Debug.Trace (trace)

import Data.Char (ord)

import Chess.Pieces
import Chess.Input
import Chess.Game
import Chess.Moves.Types


isValidKingMove :: MoveValidator
isValidKingMove gameStatus input
  | isValidKingStep = trace ("King OK: " ++ show (getOrigin input) ++ " -> " ++ show (getTarget input)) gameStatus
  | otherwise       = trace ("King FAIL: " ++ show (getOrigin input) ++ " -> " ++ show (getTarget input) ++ " oneSq=" ++ show (isOneSquareMove input) ++ " dir=" ++ show (isValidDirection input)) $ pushError gameStatus InvalidMove
  where
    isValidKingStep = isOneSquareMove input && isValidDirection input


isOneSquareMove :: Input -> Bool
isOneSquareMove input =
    let originCoord = cellToCoordinate (getOrigin input)
        targetCoord = cellToCoordinate (getTarget input)
        fileDiff = abs (fst originCoord - fst targetCoord)
        rankDiff = abs (snd originCoord - snd targetCoord)
     in fileDiff <= 1 && rankDiff <= 1 && (fileDiff + rankDiff > 0)


isValidDirection :: Input -> Bool
isValidDirection input =
    let originCoord = cellToCoordinate (getOrigin input)
        targetCoord = cellToCoordinate (getTarget input)
        fileDiff = abs (fst originCoord - fst targetCoord)
        rankDiff = abs (snd originCoord - snd targetCoord)
     in (fileDiff == 1 && rankDiff == 1)   -- diagonal
     || (fileDiff == 1 && rankDiff == 0)   -- horizontal
     || (fileDiff == 0 && rankDiff == 1)   -- vertical


reachableKingCoordinates :: PathComputerCallback
reachableKingCoordinates (m, n) =
    [(x, y) | x <- [m-1, m, m+1], y <- [n-1, n, n+1], isOnTheBoard (x, y), (x, y) /= (m, n)]
