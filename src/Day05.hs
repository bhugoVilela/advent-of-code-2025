{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

module Day05 where

import Data.List (sortBy)
import Data.Function (on)

type Range = (Int, Int)

data Database = Database
  { ranges :: [Range]
  , ids :: [Int]
  } deriving (Show)

part1 :: String -> Int
part1 = solvePart1 . parse

part2 :: String -> Int
part2 = solvePart2 . parse

solvePart1 :: Database -> Int
solvePart1 (Database ranges ids) = length $ filter isFresh ids
  where
  isFresh :: Int -> Bool
  isFresh id = any (contains id) ranges

  contains :: Int -> Range -> Bool
  contains id (min, max) = id >= min && id <= max


-- | Part2 is easy if you decide to sort the ranges by the start
-- | doing that means each nextRange never starts before a previous range
-- | which allows you to at each point keep track of the minimumValue to start counting from
solvePart2 :: Database -> Int
solvePart2 = snd . foldl' addRange empty . sortBy (compare `on` fst) . ranges
  where
  countRange (min, max) = max - min + 1

  addRange :: (Range, Int) -> Range -> (Range, Int)
  addRange ((_, prevMax), count) (nextMin, nextMax) = 
    let newRange = ( max nextMin (prevMax + 1), max nextMax prevMax)
     in (newRange, count + countRange newRange)
  empty :: (Range, Int)
  empty = ((undefined, (-2)), 0)

parse :: String -> Database
parse str =
  let rows = lines str
      (rangeLines, idLines) = break (null) rows
   in Database
        (map parseRange rangeLines)
        (map read $ tail idLines)
  where
  parseRange :: String -> Range
  parseRange str = let (fst', snd') = break (== '-') str
                    in (read fst', read . tail $ snd')
