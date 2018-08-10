module Main where

import Data.Char ( toUpper )
import Control.Monad ( void )

import Text.Parsec
import Text.Parsec.String

data Device = Chip String | Gen String
  -- deriving Show

instance Show Device where
  show (Chip s) = toUpper (head s) : "M"
  show (Gen s) = toUpper (head s) : "G"

data Floors = Floors [[Device]]
  deriving Show

-- instance Show Floors where
--   show (Floors fs) = unlines . map showFloor . zip [4,3,2,1] . reverse $ fs
--     where
--       showFloor (n,ds) = intercalate " " $ ('F' : show n) : map showDevice ds
--       showDevice =

main = print =<< readInput "input.txt"

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
