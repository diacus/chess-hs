module Chess where

import Chess.Types
import Chess.Engine

initialBoard :: Board
initialBoard = [(('a', 8), (Piece Rook   Black)),
                (('b', 8), (Piece Knight Black)),
                (('c', 8), (Piece Bishop Black)),
                (('d', 8), (Piece Queen  Black)),
                (('e', 8), (Piece King   Black)),
                (('f', 8), (Piece Bishop Black)),
                (('g', 8), (Piece Knight Black)),
                (('h', 8), (Piece Rook   Black)),
                (('a', 7), (Piece Pawn   Black)),
                (('b', 7), (Piece Pawn   Black)),
                (('c', 7), (Piece Pawn   Black)),
                (('d', 7), (Piece Pawn   Black)),
                (('e', 7), (Piece Pawn   Black)),
                (('f', 7), (Piece Pawn   Black)),
                (('g', 7), (Piece Pawn   Black)),
                (('h', 7), (Piece Pawn   Black)),

                (('a', 1), (Piece Rook   White)),
                (('b', 1), (Piece Knight White)),
                (('c', 1), (Piece Bishop White)),
                (('d', 1), (Piece Queen  White)),
                (('e', 1), (Piece King   White)),
                (('f', 1), (Piece Bishop White)),
                (('g', 1), (Piece Knight White)),
                (('h', 1), (Piece Rook   White)),
                (('a', 2), (Piece Pawn   White)),
                (('b', 2), (Piece Pawn   White)),
                (('c', 2), (Piece Pawn   White)),
                (('d', 2), (Piece Pawn   White)),
                (('e', 2), (Piece Pawn   White)),
                (('f', 2), (Piece Pawn   White)),
                (('g', 2), (Piece Pawn   White)),
                (('h', 2), (Piece Pawn   White))]

putGameStatus :: GameStatus -> IO()
putGameStatus = putStrLn . showGameStatus

playLoop :: GameStatus -> IO ()
playLoop gameStatus =
    do
        putGameStatus gameStatus
        input <- getLine
        case input of
          "." -> putStrLn "bye"
          _   -> let nextStatus = applyInput gameStatus input
                  in playLoop nextStatus
        return ()

play :: IO ()
play = playLoop (GameStatus initialBoard White)
