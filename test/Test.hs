import System.IO
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Test.HUnit

import Test.Chess.Engine (engineTests)
import Test.Chess.Input  (testInput)

tests = TestList $ engineTests
                ++ testInput

main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runTestTT tests
