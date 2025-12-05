{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import qualified Data.Text as T
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List (sort, nub)

type Range a = (a, a)

part1 :: String -> Int
part1 = T.pack 
      >>> parse 
      >>> concatMap invalidIdsInRange 
      >>> sum 
  where
    invalidIdsInRange :: Range T.Text -> [Int]
    invalidIdsInRange (a, b) = [T.length a .. T.length b]
                               & filter even                          -- ^Only even length numbers, odd ones cannot be mirrored
                               & concatMap (invalidIdsByNumLength !!) -- ^Get all invalidIds between those magnitudes
                               & dropWhile (< textRead @Int a)
                               & takeWhile (<= textRead @Int b) 

    -- infinite list of all possible invalidIds by magnitude
    invalidIdsByNumLength :: [[Int]]
    invalidIdsByNumLength = [0..] 
      & map (
          -- |List of invalidIds for this numLength, we generate the first half and then duplicate it
         map (read @Int) . map (\a -> a <> a) . getNDigits  . (`div` 2)
      )

part2 :: String -> Int
part2 = T.pack 
      >>> parse 
      >>> concatMap invalidIdsInRange
      >>> sum 
  where
    invalidIdsInRange :: Range T.Text -> [Int]
    invalidIdsInRange (a, b) = [T.length a .. T.length b] 
                               & concatMap (invalidIdsByNumLength !!)
                               & dropWhile (< textRead @Int a)
                               & takeWhile (<= textRead @Int b) 
    -- | list of invalidIds by length of number
    invalidIdsByNumLength :: [[Int]]
    invalidIdsByNumLength = [0..] & map getInvalidIds
      where getInvalidIds numLen =
              let prefixLengths = filter (\prefixLen -> numLen `rem` prefixLen == 0) [1..(numLen - 1)]
                  -- | how many times we need to repeat a prefix to get back at numLen
                  prefixRepeats prefixLen = numLen `quot` prefixLen 
                  invalidIds = prefixLengths 
                    -- |generate all invalidIds by repeating the prefixes as needed
                    & map (\prefixLen -> map (read @Int . concat . replicate (prefixRepeats prefixLen)) $ getNDigits prefixLen)
               in sort . nub . concat $ invalidIds

    numbersByMagnitude :: [[Int]]
    numbersByMagnitude = [0..] & map (map (read @Int) . getNDigits)

-- | ie. magnitude 100 = 3
numLength :: Int -> Int
numLength = length . show

parse :: T.Text -> [Range T.Text]
parse = T.splitOn "," >>> map parseRange
  where 
    parseRange str = let [a, b] = T.splitOn "-" str
                      in (a, b)


-- |Returns all Numbers with n digits in asc order
getNDigits :: Int -> [[Char]]
getNDigits numChars = 
  let first = ['1'..'9']
      other = ['0'..'9']
      all = first : replicate (numChars - 1) other
   in map reverse $ foldl' go ([""]) all
   where
    go :: [String] -> String -> [String]
    go acc digits = flip concatMap acc $ \str -> map (:str) digits

textRead :: (Read a) => T.Text -> a
textRead = read . T.unpack 
