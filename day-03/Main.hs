module Main where

import Data.List ( transpose, unfoldr )

main =
  do input <- readInput
     print . length . filter triangle $ input
     print . length . filter triangle $ chopEvery 3 . concat . transpose $ input

readInput = map (map read . words) . lines <$> readFile "input.txt"

triangle [a,b,c] = a+b > c && b+c > a && c+a > b

chopEvery n = unfoldr chop
  where
    chop [] = Nothing
    chop xs = Just (take n xs, drop n xs)
