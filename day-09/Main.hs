{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Parsec
import Text.Parsec.Char

import Debug.Trace

import Control.Monad ( replicateM )

main = tests

tests = mapM_ (print . test) =<< (map words . lines) <$> readFile "tests.txt"

test [input,desired] = ( output == desired , input , desired , output )
  where
    output = decompress input

decompress input =
  case parse p "input" input of
    Right res -> res
    Left err -> traceShow err ""

num :: Parsec String () Int
num = read @Int <$> many1 digit

p :: Parsec String () String
p = fmap concat $ many1 $
      try (many1 upper)
      <|>
      try (do { char '('
              ; n <- num
              ; char 'x'
              ; reps <- num
              ; char ')'
              ; chars <- replicateM n anyChar
              ; pure . concat . replicate reps $ chars
              })
