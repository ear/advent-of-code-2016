module Main where

import Data.Bifunctor

import Crypto.Hash

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

md5 :: ByteString -> Digest MD5
md5 = hash

doorIDs, interestingHashes :: ByteString -> [(Integer,ByteString)]

doorIDs s = (\n -> (n,BS.append s . BS.pack . show $ n)) <$> [0..]

interestingHashes s = filter ((== BS.pack "00000") . BS.take 5 . snd)
                    $ second (BS.pack . show . md5) <$> doorIDs s

main :: IO ()
main =
  do input <- BS.readFile "input.txt"
     let passHashes = take 8 (interestingHashes input)
     mapM_ print passHashes
     putStrLn $ BS.head . snd . BS.splitAt 5 . snd <$> passHashes
