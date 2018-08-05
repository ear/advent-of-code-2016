{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Parsec

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Bifunctor

import Debug.Trace

main = do
  instructions <- readInput
  print . Set.size . foldl (\s f -> trace (pp $ eval f s) $! traceShow f $! (eval f s)) Set.empty $ instructions

eval :: Instr -> (Screen -> Screen)
eval (Rect w h) = rect w h
eval (RotateRow y n) = rotateRow y n
eval (RotateCol x n) = rotateCol x n

type Screen = Set (Int,Int)

sw = 50
sh = 6

rect, rotateRow, rotateCol :: Int -> Int -> (Screen -> Screen)
rect w h = Set.union $ Set.fromList [ (x,y) | y <- [0..(h-1)], x <- [0..(w-1)] ]
rotateRow y n s = (first (rot sw n) `Set.map` row) `Set.union` s'
  where
    (row,s') = Set.partition ((y ==) . snd) s
rotateCol x n s = (second (rot sh n) `Set.map` col) `Set.union` s'
  where
    (col,s') = Set.partition ((x ==) . fst) s

rot bound n = (`mod` bound) . (n +)

data Instr
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving Show

readInput = do Right x <- parse p "input.txt" <$> readFile "input.txt"; return x

p = pinstr `sepEndBy1` newline
  where
    pinstr = try prect <|> try protrow <|> protcol
    prect = do string "rect "
               w <- read @Int <$> many1 digit
               char 'x'
               h <- read @Int <$> many1 digit
               return $ Rect w h
    protrow = do string "rotate row y="
                 y <- read @Int <$> many1 digit
                 string " by "
                 n <- read @Int <$> many1 digit
                 return $ RotateRow y n
    protcol = do string "rotate column x="
                 x <- read @Int <$> many1 digit
                 string " by "
                 n <- read @Int <$> many1 digit
                 return $ RotateCol x n

pp :: Screen -> String
pp s = concat [ [ there x y | x <- [0..sw-1] ] ++ "\n" | y <- [0..sh-1] ]
  where there x y | (x,y) `Set.member` s = '#' | otherwise = '.'
