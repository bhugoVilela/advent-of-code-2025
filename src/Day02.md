# Day 2
## Part 1

For part 1 we're given a list of integer ranges (N-M) and are asked to count invalid ids
where an id is invalid if the first part of it is equal to the second (eg. 123123)

A few cerimonies

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import qualified Data.Text as T
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List (sort, nub)
```

As always, we start with parsing.<br/>
A tuple seems to be a fitting choice for a range.
I chose to parse into a Tuple of Texts because it's easier to manipulate the numbers.
Range didn't need to be polymorphic but alas, as it was written it stays.

```haskell
type Range a = (a, a)

parse :: T.Text -> [Range T.Text]
parse = T.splitOn "," >>> map parseRange
  where 
    parseRange str = let [a, b] = T.splitOn "-" str
                      in (a, b)
```

Okay time to think this through... let's say we have the following range
`(101-9999999999)` that's a lot of numbers to check, what if instead we
looked at numbers by their length?
We know we can skip all numbers with odd lenghts 100-999 for example are invalid.
What about the others? Well we only need to look at the first half.
So for all the numbers with length 4 (ie. 9898) we can just list all 2-digit numbers
and then duplicate them (ie. 98 -> 9898). 
<br><br>
**`invalidIdsInRange`** gets all the invalid Ids in the lengths of range and keeps only those within bounds
<br><br>
**`invalidIdsByNumLength`** does the magic. Given a length (ie. 4) gets all possible numbers with half that length (ie. 2)
  and generates invalidIds by duplicating them
<br><br>
and VOILA we got ourselves a part1 done.


```haskell
part1 :: String -> Int
part1 = T.pack 
      >>> parse 
      >>> concatMap invalidIdsInRange 
      >>> sum 
  where
    invalidIdsInRange :: Range T.Text -> [Int]
    invalidIdsInRange (a, b) = [T.length a .. T.length b]
                               & filter even                          -- ^Only even length numbers, odd ones cannot be mirrored
                               & concatMap (invalidIdsByNumLength !!) -- ^Get all invalidIds between those lengths
                               & dropWhile (< textRead @Int a)
                               & takeWhile (<= textRead @Int b) 

    -- infinite list of all possible invalidIds by length
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

    numbersByLength :: [[Int]]
    numbersByLength = [0..] & map (map (read @Int) . getNDigits)

-- | ie. numLength 100 = 3
numLength :: Int -> Int
numLength = length . show

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
```
