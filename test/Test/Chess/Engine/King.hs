module Test.Chess.Engine.King (kingTests) where

import Test.HUnit

import Chess.Game
import Chess.Engine

kingTests = [TestLabel "King with empty board" testKingOnEmptyGame]

testKingOnEmptyGame =
  TestCase (assertEqual "King with empty board" False False)
