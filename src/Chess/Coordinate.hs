module Chess.Coordinate where

import Data.Char (chr, ord)

import Chess.Board

type Coordinate = (Int, Int)
type PathComputerCallback = (Coordinate -> [Coordinate])

cellToCoordinate :: BoardCell -> Coordinate
cellToCoordinate (file, rank) = (ord file - ord 'a', rank - 1)

coordinateToCell :: Coordinate -> BoardCell
coordinateToCell (x, y) = (chr (x + ord 'a'), y + 1)

isOnTheBoard :: Coordinate -> Bool
isOnTheBoard (x, y) = elem x [0 .. 7] && elem y [0 .. 7]

