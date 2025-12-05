{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day03 where

import Control.Arrow ((>>>))
import Data.Function ((&), on)
import Data.Foldable (Foldable(foldMap'))
import Data.Monoid (Sum(Sum, getSum))
import Data.List (sortBy)

type Bank = [Int]

part1 :: String -> Int
part1 = solve . parse
  where
    solve :: [Bank] -> Int
    solve = sum . map getBankJoltage

    getBankJoltage :: Bank -> Int
    getBankJoltage = maximum . allJoltagesInBank

    allJoltagesInBank :: Bank -> [Int]
    allJoltagesInBank = map (\(a, b) -> a * 10 + b) . batteryPairs

    batteryPairs :: Bank -> [(Int, Int)]
    batteryPairs (x:xs) = map (x,) xs <> batteryPairs xs
    batteryPairs [] = []


-- | The solution for part2 is more efficient and could be used for part1 by using (getMaxJoltageOf 2)
part2 :: String -> Int
part2 = solve . parse
  where
    solve :: [[Int]] -> Int
    solve = sum . map (getMaxJoltageOf 12)

    -- |returns the max joltage of taking `size` batteries in a bank
    getMaxJoltageOf :: Int -> [Int] -> Int
    getMaxJoltageOf size digits = mergeDigits $ reverse $ go 0 0 []
      where
        go :: Int -> Int -> [Int] -> [Int]
        go n _ acc | n == size = acc
        -- | recursively find the next best digit
        -- when picking a digit we know what positions in the original array are valid
        -- we pick the greatest digit and if there are multiple we pick the first
        go ix minBound acc = let digitBounds = [minBound..(length digits - size + ix)]
                                 (maxDigitIx, maxDigit) = head . sortBy (compare `on` negate . snd) $ map (\(ix, ix2) -> (ix, digits!!ix2)) $ zip digitBounds digitBounds
                              in go (ix + 1) (maxDigitIx +1) (maxDigit:acc)


parse :: String -> [[Int]]
parse = lines 
      >>> map (map (read @Int . (:[])))

-- | Merges digits into number. 
-- mergeDigits [1, 2, 3] === 123
mergeDigits :: [Int] -> Int
mergeDigits xs = zip [0..] (reverse xs) 
          & foldMap' (\(ix, d) -> Sum (d * 10 ^ ix))
          & getSum

