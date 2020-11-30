{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

main =
  do vm1 <- load . parse <$> getRawInput 12
     print (_regs (run vm1) M.! 'a')
     let vm2 = vm { _regs = M.insert 'c' 1 (_regs vm) }
     print (_regs (run vm2) M.! 'a')
  where
    parse = map (instr . words) . lines
    instr ["cpy", src, dst]
      | all isDigit src = CopyImm (read src) (head dst)
      | otherwise       = CopyReg (head src) (head dst)
    instr ["inc", reg] = Inc (head reg)
    instr ["dec", reg] = Dec (head reg)
    instr ["jnz", src, dst]
      | all isDigit src = JnzImm (read src) (read dst)
      | otherwise       = JnzReg (head src) (read dst)

data OP
  = CopyImm Int  Char
  | CopyReg Char Char
  | Inc Char
  | Dec Char
  | JnzImm Int  Int
  | JnzReg Char Int
  deriving (Show)

data VM
  = VM { _len :: Int
       , _prgm :: IntMap OP
       , _regs :: Map Char Int
       , _ip :: Int
       } deriving (Show)

load prgm
  = VM { _len = length prgm
       , _prgm = IM.fromList (zip [0..] prgm)
       , _regs = M.fromList (zip "abcd" (repeat 0))
       , _ip = 0
       }

run vm@VM{..}
  -- end
  | _ip >= _len = vm

  | otherwise
    = case _prgm IM.! _ip of
        CopyImm n   dst -> run . incIp $ vm { _regs = M.insert dst n _regs }
        CopyReg src dst -> run . incIp $ vm { _regs = M.insert dst (_regs M.! src) _regs }

        Inc reg         -> run . incIp $ vm { _regs = M.update (Just . succ) reg _regs }
        Dec reg         -> run . incIp $ vm { _regs = M.update (Just . pred) reg _regs }

        JnzImm 0          _   -> run . incIp $ vm
        JnzImm _          off -> run $ vm { _ip = _ip + off }

        JnzReg (reg -> 0) _   -> run . incIp $ vm
        JnzReg (reg -> _) off -> run $ vm { _ip = _ip + off }

  where
    incIp vm@VM{..} = vm { _ip = _ip + 1 }
    reg = (_regs M.!)
