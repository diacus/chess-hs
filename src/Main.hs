module Main where

import System.IO
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Chess (play)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  play
