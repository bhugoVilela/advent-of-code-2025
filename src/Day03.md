# Day 3
## Part 1

### The problem
Our puzzle input this time is a list of Int
```md
987654321111111
811111111111119
234234234234278
818181911112111
```
each row is a bank of batteries
our task is to find the maximum joltage for each bank and sum all of them

In part 1 the maximum joltage of the bank is the biggest number resulting from picking any 2-digits in the bank
(without changing their order)

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day03 where

import Control.Arrow ((>>>))
import Data.Function ((&), on)
import Data.Foldable (Foldable(foldMap'))
import Data.Monoid (Sum(Sum, getSum))
import Data.List (sortBy)
```

Our model today is a `[[Int]]` we're concerning ourselves with digits today.

```haskell
type Bank = [Int]
```

Parsing is straighforward, we split the string into lines and convert each char in each line to an Int

```haskell
parse :: String -> [[Int]]
parse = lines 
      >>> map (map (read @Int . (:[])))
```

we calculate the joltage of each bank by generating all possible `batteryPairs`
getting their joltage and picking the greatest.

then we just sum and that's it

```haskell
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
```

## Part 2

Part 2 is a bit more interesting, this time we need to pick 12 batteries to find the joltage.

The key of this algorithm is starting backwards, to make it simple, let's imagine a bank with 6 batteries where we need to pick 4 to find the joltage.

Our first battery can be anywhere but the last 3 places, 
because in the "worst case scenario" that the digits are in ascending order
we still need 3 batteries after the first.

The keypoint here is that we can greedily pick the greatest digit of those available, because we're always picking the next most significant digit.
In case there's a tie we should pick the first one to give more choice to the next digits
```haskell
```

Most of part 2 is the same as part 1 except we have `getMaxJoltageOf`
which takes the number of batteries needed to form a joltage and a bank and returns the maximum joltage

```haskell
-- | The solution for part2 is more efficient and could be used for part1 by using (getMaxJoltageOf 2)
part2 :: String -> Int
part2 = solve . parse
  where
    solve :: [[Int]] -> Int
    solve = sum . map (getMaxJoltageOf 12)
```

For each digit we find the available positions and the return the greatest number in the earliest position.

We achieve this through a fold keeping track of the greatest digit so far and its position

```haskell
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
```

To merge digits into a number we just reverse them a multiply by 10 to the power of its index.

The traditional algorithm
```haskell
-- | Merges digits into number. 
-- mergeDigits [1, 2, 3] === 123
mergeDigits :: [Int] -> Int
mergeDigits xs = zip [0..] (reverse xs) 
          & foldMap' (\(ix, d) -> Sum (d * 10 ^ ix))
          & getSum

```
