{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Parsec ( parse, many1, try, (<|>), digit, upper, char )
import Text.Parsec.Char ( anyChar )
import Text.Parsec.String ( Parser )

import Debug.Trace ( traceShow )

import Control.Monad ( replicateM )

main = do
  tests
  print . length . decompress =<< readFile "input.txt"

tests = mapM_ (print . test) =<< (map words . lines) <$> readFile "tests.txt"

test [input,desired] = ( output == desired , input , desired , output )
  where
    output = decompress input

decompress input =
  case parse p "input" input of
    Right res -> res
    Left err -> traceShow err ""

num :: Parser Int
num = read @Int <$> many1 digit

p :: Parser String
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
