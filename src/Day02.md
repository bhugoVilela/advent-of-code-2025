# Day 2
## Part 1

Today's problem was easier than expected, for part 1 we get a list of ranges. ie.

```md
(90 - 10000)
(57 - 82345)
```
We need to find all invalidIds between each range and sum them.
An id is invalid if it's first and second half are equal (ie. 1010, 123123, 88).

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import qualified Data.Text as T
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List.Split (chunksOf)
```

As always, we start with parsing the input, a list of `(Int, Int)` seems fitting

```haskell
type Range = (Int, Int)

parse :: String -> [Range]
parse = T.pack >>> T.splitOn "," >>> map parseRange
  where 
    parseRange str = let [a, b] = T.splitOn "-" str
                      in (read @Int $ T.unpack a, read @Int $ T.unpack b)
```

Put simply, for each range we get the invalid ids and sum them

`invalidIdsInRange` - checks all numbers in a range, filters only the invalid ones and sums them 

`isInvalid` - returns True if the number has even length and its first and second half are equal

There really isn't much to see here

```haskell
part1 :: String -> Int
part1 = parse >>> map invalidIdsInRange >>> sum 
  where
    invalidIdsInRange :: Range -> Int
    invalidIdsInRange (a, b) = sum $ filter isInvalid $ [a..b]
    isInvalid :: Int -> Bool
    isInvalid n = let str = show n
                      [fst, snd] = chunksOf (length str `div` 2) str
                   in even (length str) && fst == snd

```

## Part 2
Part 2 bumps the difficulty slightly now an id is invalid if it consists of a number repeated 2 or more times, ie.

- 111 (1 repeated 3 times)
- 123123123 (123 repeated 3 times)
- 88 (8 repeated 2 times)

**Takeaway**: we can check for divisors of the length of the string, break the string into that many chunks and check if all chunks are equal

The only change here is in `isInvalid` where we get the divisors of length of number, and check if any of the divisors holds the property above

```haskell
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
```
