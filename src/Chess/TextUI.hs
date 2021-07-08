module Chess.TextUI where

import Data.Foldable

import Chess.Pieces

showTheBoard = linesToText . mergePicesAndBorders

mergePicesAndBorders :: [[Piece]] -> [[Char]]
mergePicesAndBorders pieces = merge borders stringPieces where
    stringPieces = map piecesToString (zip [1..] pieces)


merge :: [a] -> [a] -> [a]
merge xs ys = goMerge xs ys []

goMerge :: [a] -> [a] -> [a] -> [a]
goMerge xs ys ms
  | null xs = ms ++ ys
  | null ys = ms ++ xs
  | otherwise = goMerge xs' ys' ms'
        where
            xs'   = tail xs
            ys'   = tail ys
            ms'   = ms ++ heads
            heads = (head xs) : (head ys) : []


linesToText :: [[Char]] -> [Char]
linesToText = 
    let header = "     a   b   c   d   e   f   g   h"
     in foldl (\a b -> a ++ "\n" ++ b) header

piecesToString :: (Int, [Piece]) -> [Char]
piecesToString (n, pieces) =  " " ++ (show n) ++ " " ++ [pipe] ++ stringPieces ++ [pipe] where
    stringPieces = foldl (\a b -> a ++ [pipe] ++ b) first rest
    first = (show . head) pieces
    rest  = map show (tail pieces)

borders = [topBorder] ++ innerBorders ++ [bottomBorder]
    where
        topBorder    = border leftUpCorner upCorner rightUpCorner
        bottomBorder = border leftDownCorner downCorner rightDownCorner
        innerBorders = repeatNTimes 7 (border leftCorner cross rightCorner)

border left middle right = "   " ++ [left] ++ crossedBorder ++ [right]
    where
        crossedBorder = foldr (\a b -> a ++ [middle] ++ b) threeDashes moreDashes
        moreDashes = repeatNTimes 7 threeDashes

repeatNTimes :: Int -> a -> [a]
repeatNTimes n = ((take n) . repeat)

threeDashes = [dash, dash, dash]

pipe = '│'

dash = '─'

leftUpCorner = '┌'

rightUpCorner = '┐'

leftDownCorner = '└'

rightDownCorner = '┘'

upCorner = '┬'

downCorner = '┴'

leftCorner = '├'

rightCorner = '┤'

cross = '┼'
