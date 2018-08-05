module Main where

import Data.Char ( digitToInt )
import Data.Bifunctor

import Crypto.Hash

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import qualified Data.Map.Strict as Map

import Debug.Trace ( traceShowId )

md5 :: ByteString -> ByteString
md5 = BS.pack . show . (hash :: ByteString -> Digest MD5)

doorIDs :: ByteString -> [(Integer,ByteString)]
doorIDs s = (\n -> ( n , s `BS.append` (BS.pack (show n)) )) <$> [0..]

interestingHashes :: ByteString -> [(Integer,ByteString)]
interestingHashes s = filter interesting $ second md5 <$> doorIDs s
  where interesting = (== BS.pack "00000") . BS.take 5 . snd

interestingHashes' :: [(Integer,ByteString)] -> [(Integer,Int,ByteString)]
interestingHashes' = filter valid . map withPositions
  where
    withPositions (n,bs) = ( n , digitToInt . BS.head . BS.drop 5 $ bs, bs )
    valid (_,p,_) = 0 <= p && p <= 7

extractPassword :: [ (Integer, Int, ByteString) ] -> String
extractPassword =
    Map.elems . head . dropWhile ((<8) . Map.size) . scanl collect Map.empty
  where
    collect m (_, pos, bs)
      | pos `Map.member` m = m
      | otherwise          = Map.insert pos char m
        where
          char = BS.head (BS.drop 6 bs)

main :: IO ()
main =
  do input <- BS.readFile "input.txt"
     let hashes = interestingHashes input
     let passHashes = take 8 hashes
     mapM_ print passHashes
     putStrLn $ BS.head . snd . BS.splitAt 5 . snd <$> passHashes
     putStrLn . extractPassword . map traceShowId . interestingHashes' $ hashes
