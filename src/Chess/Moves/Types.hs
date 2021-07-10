module Chess.Moves.Types where

import Data.Char (chr, ord)

import Chess.Input
import Chess.Game

type Coordinate = (Int, Int)
type PathComputerCallback = (Coordinate -> [Coordinate])
type MoveValidator = (GameStatus -> Input -> Bool)


isTargetInScope :: PathComputerCallback -> GameStatus -> Input -> Bool
isTargetInScope getReachableCoords gs input = elem target scope
    where
        target = cellToCoordinate toCell
        scope  = getReachableCoords start
        start  = cellToCoordinate fromCell
        (Input _ fromCell toCell) = input


cellToCoordinate :: BoardCell -> Coordinate
cellToCoordinate (file, rank) = (ord file - ord 'a', rank - 1)

coordinateToCell :: Coordinate -> BoardCell
coordinateToCell (x, y) = (chr (x + ord 'a'), y + 1)

isOnTheBoard :: Coordinate -> Bool
isOnTheBoard (x, y) = elem x [0 .. 7] && elem y [0 .. 7]
