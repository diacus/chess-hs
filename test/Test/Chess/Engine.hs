module Test.Chess.Engine (engineTests) where

import Test.HUnit

import Test.Chess.Engine.King   (kingTests)
import Test.Chess.Engine.Queen  (queenTests)
import Test.Chess.Engine.Errors (errorTests)
import Test.Chess.Engine.Rook   (rookTests)
import Test.Chess.Engine.Pawn   (pawnTests)

engineTests = kingTests ++ rookTests ++ queenTests ++ pawnTests ++ errorTests
