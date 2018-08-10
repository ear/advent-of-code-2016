{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Maybe ( catMaybes )

import qualified Data.Set as Set

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import Control.Applicative ( (<|>) )
import Control.Monad ( guard, foldM )

import Text.Parsec ( try, many1, sepEndBy1, string, digit, char, newline )
import Text.Parsec.String

import Data.Bifunctor

import Debug.Trace

-- Input

data Output
  = ToBot Int
  | ToOut Int
  deriving Show

data Assign = Assign
  { _val :: Int
  , _outId :: Output
  } deriving Show

data Spec = Spec
  { _from :: Int
  , _lowTo :: Output
  , _highTo :: Output
  } deriving Show

p :: Parser ([Spec],[Assign])
p = bimap concat concat . unzip <$> (pline `sepEndBy1` newline)
  where

    pline =
      try (do { string "bot "
              ; n <- num
              ; string " gives low to "
              ; ltype <- try (string "bot") <|> string "output"
              ; char ' '
              ; l <- num
              ; string " and high to "
              ; htype <- try (string "bot") <|> string "output"
              ; char ' '
              ; h <- num
              ; return ([spec n ltype l htype h],[]) })
      <|>
      do { string "value "
         ; val <- num
         ; string " goes to bot "
         ; id <- num
         ; return ([],[Assign val (ToBot id)]) }

    num = read @Int <$> many1 digit

    spec n "bot"    l "bot"    h = Spec n (ToBot    l) (ToBot    h)
    spec n "bot"    l "output" h = Spec n (ToBot    l) (ToOut h)
    spec n "output" l "bot"    h = Spec n (ToOut l) (ToBot    h)
    spec n "output" l "output" h = Spec n (ToOut l) (ToOut h)

-- Bots

data Bot = Bot
  { _id :: Int
  , _low :: Maybe Int
  , _high :: Maybe Int
  , _lowDest :: Output
  , _highDest :: Output
  } deriving Show

mkBot :: Spec -> Bot
mkBot Spec{..} = Bot
  { _id = _from
  , _low = Nothing -- Just _from
  , _high = Nothing
  , _lowDest = _lowTo
  , _highDest = _highTo
  }

botAssign :: Int -> Bot -> Bot
botAssign _ b | Bot _ (Just _)   (Just _)    _ _ <- b = error "bot already full"
botAssign n b | Bot _ Nothing    Nothing     _ _ <- b = b { _low = Just n }
botAssign n b | Bot _ (Just low) Nothing     _ _ <- b =
  if n > low
    then b {                   _high = Just n   }
    else b { _low = Just n   , _high = Just low }
botAssign n b | Bot _ Nothing    (Just high) _ _ <- b =
  if n < high
    then b { _low = Just n                      }
    else b { _low = Just high, _high = Just n   }

-- Network

data Network = Network
  { _bots :: Map Int Bot
  , _outputs :: Map Int (Maybe Int)
  , _answerKeys :: (Int,Int)
  , _answer :: Maybe Int
  } deriving Show

emptyNet = Network
  { _bots = Map.empty
  , _outputs = Map.empty
  , _answerKeys = (17,61)
  , _answer = Nothing
  }

fromList :: [Spec] -> Network
fromList specs = emptyNet { _bots = Map.fromList bs
                          , _outputs = Map.fromList os
                          }
  where

    bs = (\s@Spec{_from} -> ( _from , mkBot s )) <$> specs

    -- bs = (\n -> ( n , newBot n )) <$> (collectBots =<< specs)

    -- collectBots Spec{..} = pure _from <> botId _lowTo <> botId _highTo
    -- botId (ToBot n) = pure n
    -- botId _ = mempty

    os = (\n -> ( n , Nothing )) <$> (collectOuts =<< specs)

    collectOuts Spec{..} = outId _lowTo <> outId _highTo
    outId (ToOut n) = pure n
    outId _ = mempty

perform :: Network -> Assign -> Network
perform n@Network{..} Assign{..}

  -- Assigning to Bots can cascade into more assigments
  | ToBot botId <- _outId =
    let Just bot = Map.lookup botId _bots
        ( bot' , answer' , assigns ) = case botAssign _val bot of
          bot' | Just l <- _low bot', Just h <- _high bot'
            -> ( bot' { _low = Nothing, _high = Nothing }
               , if (l,h) == (_answerKeys) then Just (_id bot') else Nothing
               , [ Assign h (_highDest bot'), Assign l (_lowDest bot') ] )
          bot'
            -> ( bot'
               , Nothing
               , [] )
        n' = n { _bots = Map.insert botId bot' _bots
               , _answer = _answer <|> answer' }
    in foldl perform n' assigns

  -- Assigning to Outputs overwrites them
  | ToOut outId <- _outId = n { _outputs = Map.insert outId (Just _val) _outputs }

main = do
  Right ( specs , assigns ) <- parseFromFile p "input.txt"
  let network = fromList specs
  print . length $ specs
  print . length . _bots $ network
  print . length $ assigns
  let history = scanl perform network assigns
  print . length $ history
  print . length $ takeWhile (\Network{..} -> _answer == Nothing) history
  let ans = head $ dropWhile (\Network{..} -> _answer == Nothing) history
  -- print $ ans
  print . _answer $ ans
  print . _outputs $ ans
  print . product . catMaybes . (\Network{..} -> Map.elems $ Map.restrictKeys _outputs (Set.fromList [0,1,2])) $ ans
