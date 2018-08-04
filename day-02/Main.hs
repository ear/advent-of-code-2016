module Main where

import Data.Char ( intToDigit )

import Data.Bifunctor -- Bifunctor (,)

readInput :: IO [String]
readInput = lines <$> readFile "input.txt"

type Coord = (Int,Int) -- keypad: from (0,0) (top left) to (2,2) (bottom right)

move :: Char -> Coord -> Coord
move 'U' = second $ max 0 . min 2 . pred
move 'D' = second $ max 0 . min 2 . succ
move 'L' =  first $ max 0 . min 2 . pred
move 'R' =  first $ max 0 . min 2 . succ

center = (1,1) :: Coord

key :: Coord -> Int
key (x,y) = 3*y + x + 1

keys :: [Coord] -> String
keys = map (intToDigit . key)

main :: IO ()
main = putStrLn . keys . tail . scanl (foldl $ flip move) center =<< readInput
