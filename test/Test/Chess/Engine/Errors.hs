module Test.Chess.Engine.Errors (errorTests) where

import Test.HUnit

import Chess.Pieces
import Chess.Game
import Chess.Engine

errorTests = [ TestLabel "Short input is not allowed" shortInput
             , TestLabel "Long  input is not allowed" longInput
             , TestLabel "Empty input is not allowed" emptyInput
             ]


shortInput = TestCase (assertEqual message expected actual)
  where rawInput   = "R"
        gameStatus = GameStatus [(('e', 2), whiteRook)] White []
        expected   = GameStatus [(('e', 2), whiteRook)] White [ShortInput]
        actual     = applyInput gameStatus rawInput
        message    = getErrorMessage rawInput ShortInput


longInput = TestCase (assertEqual message expected actual)
  where rawInput   = "Reeeeee"
        gameStatus = GameStatus [(('e', 2), whiteRook)] White []
        expected   = GameStatus [(('e', 2), whiteRook)] White [LongInput]
        actual     = applyInput gameStatus rawInput
        message    = getErrorMessage rawInput LongInput


emptyInput = TestCase (assertEqual message expected actual)
  where gameStatus = GameStatus [] White []
        expected   = GameStatus [] White [EmptyInput]
        actual     = applyInput gameStatus ""
        message    = getErrorMessage "" EmptyInput


getErrorMessage :: [Char] -> ChessError -> [Char]
getErrorMessage input expectedError =
  "input '" ++ input ++ "' should raise '" ++ (show expectedError) ++ "' error"
