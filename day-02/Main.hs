module Main where

import Data.Char ( intToDigit, toUpper )

import Data.Bifunctor ( first, second ) -- Bifunctor (,)

readInput :: IO [String]
readInput = lines <$> readFile "input.txt"

type Coord = (Int,Int)

-- part 1 keypad: from (0,0) (top left) to (2,2) (bottom right)

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

-- part 2 keypad

dist' (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

center' = (2,2) :: Coord

move' :: Char -> Coord -> Coord
move' direction c0
  | dist' center' c1 > 2 = c0
  | otherwise            = c1

  where

    c1 = go direction c0

    go 'U' = second pred
    go 'D' = second succ
    go 'L' = first pred
    go 'R' = first succ

key' :: Coord -> Int
key' (2,0) = 1
key' (x,1) = 1+x
key' (x,2) = 5+x
key' (x,3) = 9+x
key' (2,4) = 13

keys' :: [Coord] -> String
keys' = map (toUpper . intToDigit . key')

main :: IO ()
main =
  do input <- readInput
     putStrLn . keys . tail . scanl (foldl $ flip move) center $ input
     putStrLn . keys' . tail . scanl (foldl $ flip move') center' $ input
