{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List ( unfoldr, tails )

data IPv7 = IPv7 { supernets :: [String], hypernets :: [String] }

empty = IPv7 [] []

addAddr, addHype :: String -> IPv7 -> IPv7
addAddr addr ip@IPv7{..} = ip { supernets = addr:supernets }
addHype addr ip@IPv7{..} = ip { hypernets = addr:hypernets }

main =
  do input <- map fromString . lines <$> readFile "input.txt"
     print . length . filter tls $ input
     print . length . filter ssl $ input

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

tls IPv7{..} = any abba supernets && all (not . abba) hypernets

abba = any (\case (a:b:c:d:_) -> a/=b && a==d && b==c; _ -> False) . tails

abas, babs :: IPv7 -> [(Char,Char)]
abas IPv7{..} = supernets >>= tails >>= aba
babs IPv7{..} = hypernets >>= tails >>= aba

aba (a:b:c:_) | a/=b && a==c = [(a,b)]
aba _ = []

ssl ip = any (\(x,y) -> (y,x) `elem` babs ip) (abas ip)
