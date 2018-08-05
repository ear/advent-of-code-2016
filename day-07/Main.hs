{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List ( unfoldr, tails )

data IPv7 = IPv7 { addrs :: [String], hypernets :: [String] }

empty = IPv7 [] []

addAddr, addHype :: String -> IPv7 -> IPv7
addAddr addr ip@IPv7{..} = ip { addrs = addr:addrs }
addHype addr ip@IPv7{..} = ip { hypernets = addr:hypernets }

main =
  do input <- readFile "input.txt"
     print . length . filter valid . map fromString . lines $ input

fromString :: String -> IPv7
fromString = go False empty
  where
    go _     ip [] = ip
    go False ip xs = case break ('[' ==) xs of
                       (xs0,[])   -> addAddr xs0 ip
                       (xs0,_:ys) -> go True (addAddr xs0 ip) ys
    go True  ip xs = case break (']' ==) xs of
                       (xs0,[])   -> addHype xs0 ip
                       (xs0,_:ys) -> go False (addHype xs0 ip) ys

valid IPv7{..} = any abba addrs && all (not . abba) hypernets

abba = any (\case (a:b:c:d:_) -> a/=b && a==d && b==c; _ -> False) . tails
