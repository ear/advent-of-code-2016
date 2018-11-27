{-# language OverloadedLists #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language TupleSections #-}

module Main where

-- 4 floors
-- elevator

-- rocks
-- chips
-- chip alone with wrong rock = fried
-- elevator must carry 1 or 2 pieces at a time (won't move if empty)
-- pieces on the elevator interact with pieces on the floors

-- bring all of the chips and rocks to the 4th floor
-- minimum number of movements to complete the goal?

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import Data.Bits
import Data.Word

import System.Random

-- data Move
--   = Up1 Piece
--   | Up2 Piece Piece
--   | Down1 Piece
--   | Down2 Piece Piece
--   deriving (Show,Ord,Eq)

-- data Piece
--   = Rock Int
--   | Chip Int
--   deriving (Show,Ord,Eq)

-- type Floor = Set Piece
-- type Floors = IntMap Floor

-- t :: Floors
-- t =
--   [ (1,[])
--   , (2,[])
--   , (3,[])
--   , (4,[]) ]

-- move :: Floors -> Move -> Floors
-- move = undefined

-- moves :: Floors -> [Move]
-- moves = undefined

--          rocks   chips
--         |-----| |-----|
-- floor 0 1 2 3 4 5 6 7 8
--     4   X       X
--     3
--     2       X       X
--     1

-- correspondence +-4

-- idea:
-- Array (Int,Int) Int
--       (floor,?) -chip +rock

--  -1  1
--  -2  2
--  -4  4
--  -8  8
-- -16 16

-- doesn't work completely
-- if a chip and a rock are on the same floor, they cancel out
-- rocks can stay on the same floor
-- chips can stay on the same floor
-- rocks and chips can only coexist if each match
--
-- 0 0 0
-- 1 1 0
-- 0 1 1
-- 1 0 1
--
--   1110 ^ 1010 = 0100    0100 & 1110 = 0100
--                         0100 & 1010 = 0000
--   1000 ^ 0001 = 1001    1001 & 1000 = 1000
--                         1001 & 0001 = 0001
--

data Piece = Rock !Int | Chip !Int

-- 32bit                            32bit
-- 10987654321098765432109876543210 10987654321098765432109876543210
newtype Floor = Floor Word64

emptyFloor = Floor zeroBits

addPiece :: Floor -> Piece -> Floor
addPiece (Floor f) (Chip n) = Floor $ setBit f n
addPiece (Floor f) (Rock n) = Floor $ setBit f $ 32 + n

removePiece :: Floor -> Piece -> Floor
removePiece (Floor f) (Chip n) = Floor $ clearBit f n
removePiece (Floor f) (Rock n) = Floor $ clearBit f $ 32 + n

rocks, chips :: Floor -> Word64
rocks (Floor f) = shiftR f 32
chips (Floor f) = f .&. (2^32 - 1)

safe :: Floor -> Bool
safe f = strayRocks == zeroBits || strayChips == zeroBits
  where
    unbalanced = rocks f `xor` chips f
    strayRocks = rocks f .&. unbalanced
    strayChips = chips f .&. unbalanced






data Building = Building
  { f1 :: {-# UNPACK #-}!Floor
  , f2 :: {-# UNPACK #-}!Floor
  , f3 :: {-# UNPACK #-}!Floor
  , f4 :: {-# UNPACK #-}!Floor
  }

-- type Hash = Word64
-- newtype ZobristHash = ZobristHash [Hash]
--   deriving (Show)

-- mkHash :: RandomGen g
--        => Int               -- Board size
--        -> Int               -- Number of different pieces
--        -> g
--        -> (ZobristHash,g)
-- mkHash b p g = take (b*p)
--              . iterate (randomR (minBound,maxBound) . snd)
--              $ (undefined,g)


main = print 2

