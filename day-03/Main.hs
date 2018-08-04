module Main where

main = print . length . filter triangle =<< readInput

readInput = map (map read) . map words . lines <$> readFile "input.txt"

triangle [a,b,c] = a+b > c && b+c > a && c+a > b
