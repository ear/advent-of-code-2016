module Main where
import Data.Ord ( comparing )
import Data.List ( transpose, sort, group, sortBy )
main = do input <- readFile "input.txt"
          putStrLn . map mostFrequent . transpose . lines $ input
          putStrLn . map leastFrequent . transpose . lines $ input
mostFrequent = head . head . reverse . sortBy (comparing length) . group . sort
leastFrequent = head . head . sortBy (comparing length) . group . sort
