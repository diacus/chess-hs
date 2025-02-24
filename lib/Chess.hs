module Chess where

import System.IO

import Chess.Game
import Chess.Engine


play :: IO ()
play = playLoop initialGameStatus

prompt gs = ((show . getPlayer) gs) ++ " >> "

playLoop :: GameStatus -> IO ()
playLoop gameStatus = do
    (putStrLn . show) gameStatus
    (putStr . prompt) gameStatus
    hFlush stdout
    input <- getLine
    case input of
      "." -> putStrLn "bye"
      _   -> let nextStatus = applyInput gameStatus input
              in playLoop nextStatus
    return ()
