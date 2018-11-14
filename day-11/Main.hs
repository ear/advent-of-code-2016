{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char ( toUpper )
import Control.Monad ( void, guard )

import Text.Parsec ( sepEndBy1, newline, string, many1, alphaNum, try, (<|>) )
import Text.Parsec.String ( Parser, parseFromFile )

import Data.List ( nub, sort, partition, null, (\\), foldl' )

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import Data.Bifunctor

import Debug.Trace

next s@State{..} = do
  e <- nextLevels
  cs <- candidates
  let s' = foldl' (move e) s cs
  guard $ safe s'
  void $! return $! traceShowId cs
  return s'
  where

    nextLevels = filter (>0) [_e + 1, _e - 1]

    candidates :: [[Int]]
    candidates = singlesAndPairs $ Set.union _fchips _fgens
      where
        Floor{..} = _floors Map.! _e

    move :: Int -> State -> Int -> State
    move e s c = s
      { _e = e
      , _floors = Map.adjust (floorAdd c) e $ Map.adjust (floorRemove c) _e _floors
      }

    floorRemove, floorAdd :: Int -> Floor -> Floor
    floorRemove c f@Floor{..}
      | c `Map.member` _chips = f { _fchips = c `Set.delete` _fchips }
      | otherwise             = f { _fgens  = c `Set.delete` _fgens  }

    floorAdd c f@Floor{..}
      | c `Map.member` _chips = f { _fchips = c `Set.insert` _fchips }
      | otherwise             = f { _fgens  = c `Set.insert` _fgens  }

singles, pairs, singlesAndPairs :: Ord a => Set a -> [[a]]

singles = map pure . Set.toAscList

pairs s = do
  x <- Set.toAscList s
  let s' = Set.filter (>x) s
  y <- Set.toAscList s'
  return [x,y]

singlesAndPairs = (++) <$> singles <*> pairs


data Floor = Floor
  { _fchips :: Set Int            -- { deviceId }
  , _fgens  :: Set Int            -- { deviceId }
  }
  deriving Show

data State = State
  { _elements  :: Map String Int  -- "element" -> elementId

  , _deviceIDs :: Map Device Int  -- Device -> deviceId
  , _devices   :: Map Int Device  -- deviceId -> Device

  , _chips     :: Map Int Int     -- chipId -> elementId
  , _gens      :: Map Int Int     -- genId -> elementId

  , _e         :: Int             -- elevator level
  , _floors    :: Map Int Floor   -- [1,2,3,4] -> Floor
  }
  deriving Show

mkState :: Floors -> State
mkState (Floors fs) = State
    { _elements = elements
    , _deviceIDs = deviceIDs
    , _devices = devices
    , _chips = chips
    , _gens = gens
    , _e = 1
    , _floors = floors
    }
  where
    elements = Map.fromList $ zip (nub . sort . map deviceElement . concat $ fs) [0..]

    deviceIDs = Map.fromList $ zip (concat fs) [0..]
    devices = Map.fromList . map swap . Map.toList $ deviceIDs where swap (a,b) = (b,a)

    (chips,gens) = bimap devElements devElements $ Map.partition isChip devices
    devElements = Map.map ((elements Map.!) . deviceElement)

    floors = Map.fromList . zip [1..] . map toFloor $ fs
    toFloor devs = Floor { _fchips = fchips, _fgens = fgens }
      where
        (fchips,fgens) = bimap devIDs devIDs $ partition isChip devs

    devIDs = Set.fromList . map (deviceIDs Map.!)

safe :: State -> Bool
safe State{..} = Map.null . snd $ Map.partition safeFloor _floors
  where
    safeFloor :: Floor -> Bool
    safeFloor Floor{..} = Set.null _fgens || null freeChipElts || null freeGenElts
      where
        chipElts = Map.elems . Map.restrictKeys _gens $ _fgens
        genElts = Map.elems . Map.restrictKeys _chips $ _fchips
        freeChipElts = chipElts \\ genElts
        freeGenElts = genElts \\ chipElts

data Device = Chip String | Gen String
  deriving (Eq, Ord, Show)

deviceElement :: Device -> String
deviceElement (Chip s) = s
deviceElement (Gen s) = s

isChip :: Device -> Bool
isChip (Chip _) = True
isChip _ = False

-- instance Show Device where
--   show (Chip s) = toUpper (head s) : "M"
--   show (Gen s) = toUpper (head s) : "G"

data Floors = Floors [[Device]]
  deriving Show

-- instance Show Floors where
--   show (Floors fs) = unlines . map showFloor . zip [4,3,2,1] . reverse $ fs
--     where
--       showFloor (n,ds) = intercalate " " $ ('F' : show n) : map showDevice ds
--       showDevice =

main = print . mkState =<< readInput "input.txt"

readInput file = do
  res <- parseFromFile p file
  case res of
    Right input -> return input
    Left err -> error (show err)

p :: Parser Floors
p = Floors <$> (floor `sepEndBy1` newline)
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
      element <- many1 alphaNum
      try (string " generator" >> return (Gen element))
        <|> (string "-compatible microchip" >> return (Chip element))

    seps :: Parser String
    seps = try (string ".") <|> try (string " and ") <|> try (string ", and ") <|> (string ", ")
