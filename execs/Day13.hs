module Main
  ( main
  ) where

import Advent.Coord
import Advent.Search
import Data.Bits (popCount)
-- import Data.Foldable (for_)

input :: Int
input = 1362 -- 10

dest :: Coord
dest = C 39 31 -- C 4 7

main :: IO ()
main = print part1

--  do [0..6] `for_` \y ->
--       do [0..9] `for_` \x ->
--            do putStr (show (loc (C y x)))
--          putStr "\n"

data Area
  = Open
  | Wall
  deriving (Eq)

loc :: Coord -> Area
loc (C y x)
  | even pop  = Open
  | otherwise = Wall
  where
    n = x*x + 3*x + 2*x*y + y + y*y + input
    pop = popCount n

instance Show Area where
  show Open = "."
  show Wall = "#"

part1 :: Int
part1 = snd . head . filter ((dest ==) . fst) $ reached
  where
    move (c,len) = [ (c',len+1) | c' <- cardinal c, loc c' == Open ]
    reached = bfs move (C 1 1,0)
