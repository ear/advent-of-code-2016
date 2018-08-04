{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Ord      ( comparing )
import Data.List     ( group, groupBy, sort, sortBy, intercalate, tails )
import Data.Char     ( ord, chr )
import Data.Function ( on )

data Room = Room { _name :: [String], _sector :: Int, _checksum :: String }
  deriving Show

main =
  do input <- readInput
     print . sum . map _sector . filter realRoom $ input
     mapM_ print . filter (roomNameContains "north") . map decrypt $ input

readInput = map (fromList . words . map replace) . lines <$> readFile "input.txt"
  where
    replace c | c `elem` "-[]" = ' ' | otherwise = c
    fromList xs = Room { _name = reverse n, _sector = read s, _checksum = c }
      where
        (c:s:n) = reverse xs

realRoom Room{..} = _checksum == (take 5 . map head . concat . map sort . groupBy ((==) `on` length) . reverse . sortBy (comparing length) . group . sort . concat $ _name)

decrypt r@Room{..} = r { _name = [decrypted] }
  where
    decrypted = intercalate " " $ map (map rotate) _name
    rotate = chr . (+ ord 'a') . (`mod` (ord 'z' - ord 'a' + 1)) . (+ _sector) . subtract (ord 'a') . ord

roomNameContains name Room{..} = any (== name) substrings
  where
    substrings = map (take (length name)) . tails . concat $ _name
