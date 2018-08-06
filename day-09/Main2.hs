{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Text.Parsec ( parse, many1, try, (<|>), digit, upper, char )
import Text.Parsec.Char ( anyChar )
import Text.Parsec.String ( Parser )

import Debug.Trace ( traceShow )

import Control.Monad ( replicateM )

import Data.List ( genericLength )

main = do
  tests
  print . decompress =<< readFile "input.txt"

tests = mapM_ (print . test) =<< (map words . lines) <$> readFile "tests-v2.txt"

test [input,read @Integer -> desired_len]
  = ( output_len == desired_len , input , desired_len , output_len )
  where
    output_len = decompress input

decompress input =
  case parse plen "input" input of
    Right res -> res
    Left err -> traceShow err (-1)

num :: Parser Int
num = read @Int <$> many1 digit

plen :: Parser Integer
plen = fmap sum $ many1 $
      try (genericLength <$> many1 upper)
      <|>
      try (do { char '('
              ; n <- num
              ; char 'x'
              ; reps <- num
              ; char ')'
              ; chars <- replicateM n anyChar
              ; let Right decompressedLen = parse plen "input" chars
              ; pure $ fromIntegral reps * decompressedLen
              })
