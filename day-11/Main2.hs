{-# language TupleSections #-}
module Main where

import Data.Char ( toUpper )
import Control.Monad ( join, void, guard )

import Text.Parsec ( sepEndBy1, newline, string, many1, alphaNum, try, (<|>) )
import Text.Parsec.String ( Parser, parseFromFile )

import Data.List ( nub, sort, partition, null, (\\), foldl' )

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import Data.Bifunctor

import Debug.Trace


data Element
  = Hy | Li | Po | Th | Pr | Ru | Co
  deriving (Eq, Ord, Show)

instance Read Element where
  readsPrec _ "hydrogen"   = [(Hy,"")]
  readsPrec _ "lithium"    = [(Li,"")]
  readsPrec _ "polonium"   = [(Po,"")]
  readsPrec _ "thulium"    = [(Th,"")]
  readsPrec _ "promethium" = [(Pr,"")]
  readsPrec _ "ruthenium"  = [(Ru,"")]
  readsPrec _ "cobalt"     = [(Co,"")]

data Device
  = M Element -- Microchip
  | G Element -- Generator
  deriving (Eq, Ord)

instance Show Device where
  show (M e) = shows e "M"
  show (G e) = shows e "G"

type Floors = Map Int [Device]


allDevices = join . Map.elems

allOnFourth = Map.fromList . pure . (4,) . allDevices


type Move = (Int,Device)

data S = S
  { _fs :: Floors
  ,
  } deriving (Show)


t = do
  fs <- readInput "test.txt"
  print $ fs
  print $ allOnFourth fs
  print $ allDevices fs

main = print =<< readInput "input.txt"


readInput file = do
  res <- parseFromFile p file
  case res of
    Right input -> return input
    Left err -> error (show err)

p :: Parser Floors
p = (Map.fromList . zip [1..4]) <$> (floor `sepEndBy1` newline)
  where
    floor :: Parser [Device]
    floor = do
      string "The "
      many1 alphaNum
      string " floor contains "
      devices

    devices =
      try (string "nothing relevant." >> return ([] :: [Device]))
      <|> sepEndBy1 device seps

    device :: Parser Device
    device = do
      string "a "
      element <- el
      try (string " generator" >> return (G element))
        <|> (string "-compatible microchip" >> return (M element))

    seps :: Parser String
    seps = try (string ".") <|> try (string " and ") <|> try (string ", and ") <|> (string ", ")

    el :: Parser Element
    el = read <$> many1 alphaNum
