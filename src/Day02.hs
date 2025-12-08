{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import qualified Data.Text as T
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List.Split (chunksOf)

type Range = (Int, Int)

parse :: String -> [Range]
parse = T.pack >>> T.splitOn "," >>> map parseRange
  where 
    parseRange str = let [a, b] = T.splitOn "-" str
                      in (read @Int $ T.unpack a, read @Int $ T.unpack b)

part1 :: String -> Int
part1 = parse >>> map invalidIdsInRange >>> sum 
  where
    invalidIdsInRange :: Range -> Int
    invalidIdsInRange (a, b) = sum $ filter isInvalid $ [a..b]
    isInvalid :: Int -> Bool
    isInvalid n = let str = show n
                      [fst, snd] = chunksOf (length str `div` 2) str
                   in even (length str) && fst == snd

part2 :: String -> Int
part2 = parse >>> map invalidIdsInRange >>> sum
  where
  invalidIdsInRange :: Range -> Int
  invalidIdsInRange (a, b) = sum $ filter isInvalid $ [a..b]
  isInvalid :: Int -> Bool
  isInvalid n = let str = show $ n
                    divs = [1..length str `div` 2] & filter (\it -> length str `mod` it == 0)
                 in any (allEqual . flip chunksOf str) divs


allEqual :: (Eq a) => [a] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual [] = True

-- | ie. magnitude 100 = 3
numLength :: Int -> Int
numLength = length . show
