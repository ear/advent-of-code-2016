{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Ord      ( comparing )
import Data.List     ( group, groupBy, sort, sortBy )
import Data.Function ( on )

data Room = Room { _name :: [String], _sector :: Int, _checksum :: String }
  deriving Show

main = print . sum . map _sector . filter realRoom =<< readInput

readInput = map (fromList . words . map replace) . lines <$> readFile "input.txt"
  where
    replace c | c `elem` "-[]" = ' ' | otherwise = c
    fromList xs = Room { _name = reverse n, _sector = read s, _checksum = c }
      where
        (c:s:n) = reverse xs

realRoom Room{..} = _checksum == (take 5 . map head . concat . map sort . groupBy ((==) `on` length) . reverse . sortBy (comparing length) . group . sort . concat $ _name)
