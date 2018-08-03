{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe ( listToMaybe )

data Direction = N | E | S | W
  deriving (Eq, Enum, Bounded, Read, Show)

order = succ $ fromEnum $ maxBound @Direction

turn :: Char -> Direction -> Direction
turn 'R' = toEnum . (`mod` order) . succ . fromEnum
turn 'L' = toEnum . (`mod` order) . pred . fromEnum

data Pos = Pos { _d :: !Direction, _x :: !Int, _y :: !Int }
  deriving (Show)

instance Eq Pos where
  (Pos _ x0 y0) == (Pos _ x1 y1) = x0 == x1 && y0 == y1

move :: Int -> Pos -> Pos
move n (Pos N x y) = (Pos N x (y+n))
move n (Pos E x y) = (Pos E (x+n) y)
move n (Pos S x y) = (Pos S x (y-n))
move n (Pos W x y) = (Pos W (x-n) y)

turn' :: Char -> Pos -> Pos
turn' c = \(Pos d x y) -> (Pos (turn c d) x y)

origin = Pos N 0 0

input = filter (`notElem` ", ") <$> readFile "input.txt"

go pos [] = pos
go pos (d:ds)
  | [(n,ds')] <- reads ds :: [(Int,String)] = go (move n . turn' d $ pos) ds'
  | otherwise = go pos (tail ds)

dist (Pos _ x0 y0) (Pos _ x1 y1) = abs (x0 - x1) + abs (y0 - y1)

main = do
  directions <- input
  print . dist origin . go origin $ directions
  mapM_ print . fmap (dist origin) . findFirstRepeat . trace origin $ directions

trace :: Pos -> String -> [Pos]
trace = go' []
  where

    go' ts p [] = ts
    go' ts p (d:ds)
      | [(n,ds')] <- reads ds :: [(Int,String)] =
        let p' = turn' d $ p
            ts' = move' n p'
        in go' (ts ++ ts') (last ts') ds'
      | otherwise = go' ts p (tail ds)

    move' :: Int -> Pos -> [Pos]
    move' n p = drop 1 $ scanl (flip move) p (replicate n 1)

findFirstRepeat :: Eq a => [a] -> Maybe a
findFirstRepeat = fmap (head . snd) . listToMaybe . filter fst . scanl check (False,[])
  where
    check (_,seen) x = (x `elem` seen, x:seen)

-- main = do
--   directions <- input
--   print . dist origin . go origin $ directions
--   mapM_ print . fmap (dist origin) . findFirstRepeat . go' [origin] $ directions

-- move' :: Int -> Pos -> [Pos]
-- move' n p = scanl (flip move) p (replicate n 1)

-- go' :: [Pos] -> String -> [Pos]
-- go' ps [] = reverse ps
-- go' ps@(p:_) (d:ds)
--   | [(n,ds')] <- reads ds :: [(Int,String)] =
--     let p' = turn' d $ p
--         ps' = scanr move p' (replicate n 1) ++ drop 1 ps
--     in go' ps' ds'
--   | otherwise = go' ps (tail ds)

-- findFirstRepeat :: Eq a => [a] -> Maybe a
-- findFirstRepeat = fmap (head . snd) . listToMaybe . filter fst . scanl check (False,[])
--   where
--     check (_,seen) x = (x `elem` seen, x:seen)
