module PiecePaths where

import Data.Char (chr, ord)
import Data.List (sort)

import Chess.Types

type Coordinate = (Int, Int)
type PathComputerCallback = (Coordinate -> [Coordinate])

cellToCoordinate :: BoardCell -> Coordinate
cellToCoordinate (file, rank) = (ord file - ord 'a', rank - 1)

coordinateToCell :: Coordinate -> BoardCell
coordinateToCell (x, y) = (chr (x + ord 'a'), y + 1)

isOnTheBoard :: Coordinate -> Bool
isOnTheBoard (x, y) = elem x [0 .. 7] && elem y [0 .. 7]

reachableCells :: PathComputerCallback -> BoardCell -> [BoardCell]
reachableCells f =
    sort . (map coordinateToCell) . (filter isOnTheBoard) . f . cellToCoordinate

emptyMoves :: PathComputerCallback
emptyMoves _ = []

kingMoves :: PathComputerCallback
kingMoves (m, n) =
    [(m + i, n + j) | i <- [-1 .. 1], j <- [-1 .. 1], (0, 0) /= (i, j)]

bishopMoves :: PathComputerCallback
bishopMoves (m, n) = let deltas = [-7 .. -1] ++ [1 .. 7]
                      in [(m+d,n+d) | d <- deltas] ++ [(m+d,n-d) | d <- deltas]

rookMoves :: PathComputerCallback
rookMoves (m, n) = let deltas = [-7 .. -1] ++ [1 .. 7]
                    in [(m,n+d) | d <- deltas] ++ [(m+d,n) | d <- deltas]

queenMoves :: PathComputerCallback
queenMoves p = bishopMoves p ++ rookMoves p

knightMoves :: PathComputerCallback
knightMoves (m, n) = let kJumps = [-2, -1, 1, 2]
                      in [(m + i, n + j) | i <- kJumps,
                                           j <- kJumps,
                                           abs i /= abs j]

whitePawnMoves :: PathComputerCallback
whitePawnMoves (m, 1) = [(m - 1, 2), (m + 1, 2), (m, 2), (m, 3)]
whitePawnMoves (m, n) = [(m - 1, n + 1), (m + 1, n + 1), (m, n + 1)]

blackPawnMoves :: PathComputerCallback
blackPawnMoves (m, 6) = [(m - 1,     5), (m + 1,     5), (m,     5), (m, 4)]
blackPawnMoves (m, n) = [(m - 1, n - 1), (m + 1, n - 1), (m, n - 1)        ]


movesCallback :: Piece -> PathComputerCallback
movesCallback (Piece King   _    ) = kingMoves
movesCallback (Piece Bishop _    ) = bishopMoves
movesCallback (Piece Rook   _    ) = rookMoves
movesCallback (Piece Knight _    ) = knightMoves
movesCallback (Piece Queen  _    ) = queenMoves
movesCallback (Piece Pawn   White) = whitePawnMoves
movesCallback (Piece Pawn   Black) = blackPawnMoves


pieceMoves :: Piece -> BoardCell -> [BoardCell]
pieceMoves piece cell = let pieceMoves = movesCallback piece
                         in reachableCells pieceMoves cell
