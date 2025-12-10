Day 3
=====
Part 1
------

[h3] The Problem

Today's input gives us rows of digits—think of each row as a "bank of batteries":

```md
987654321111111
811111111111119
234234234234278
818181911112111
```

Each digit represents a battery, and our job is to calculate the maximum "joltage"
for each bank, then sum them all together.

For Part 1, the joltage of a bank is determined by picking any two digits (in order,
no reversing) and forming a two-digit number. For example, from `987654321111111`,
we could pick the `9` and `8` to get `98`, or the `9` and `7` to get `97`. The maximum
joltage is the largest such number we can form.

\begin{code}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day03 where

import Control.Arrow ((>>>))
import Data.Function ((&), on)
import Data.Foldable (Foldable(foldMap'))
import Data.Monoid (Sum(Sum, getSum))
import Data.List (sortBy)
\end{code}

[h3] The Solution

Our data model is simple: a list of banks, where each bank is a list of digit values.

\begin{code}
type Bank = [Int]
\end{code}

Parsing is straightforward—we split into lines and convert each character to a single digit.

\begin{code}
parse :: String -> [[Int]]
parse = lines
      >>> map (map (read @Int . (:[])))
\end{code}

The algorithm: generate all possible pairs of digits (maintaining order), form two-digit
numbers from each pair, and take the maximum. Sum across all banks for the final answer.

\begin{code}
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
\end{code}

Part 2
------

Part 2 escalates things: now we need to pick **12 digits** to form a 12-digit number,
and maximize *that*.

[h3] The Greedy Insight

Here's the key insight: we can solve this greedily by picking digits from most significant
to least significant.

Let's think through a simpler example: picking 4 digits from a 6-digit bank. The first
digit we pick can't be in the last 3 positions—we need to leave room for the 3 digits
that follow. Among the valid positions (first 3 positions), we pick the largest digit.

If there's a tie, we pick the *earliest* occurrence. Why? Because choosing earlier gives
us more options for subsequent digits—we preserve flexibility down the line.

Once we've picked the first digit, we repeat the process for the second digit (starting
from just after our first pick), then the third, and so on.

[h3] Implementation

The structure mirrors Part 1, except we use `getMaxJoltageOf` to generalize to any
number of digits (12 in this case).

\begin{code}
-- | The solution for part2 is more efficient and could be used for part1 by using (getMaxJoltageOf 2)
part2 :: String -> Int
part2 = solve . parse
  where
    solve :: [[Int]] -> Int
    solve = sum . map (getMaxJoltageOf 12)
\end{code}

The `getMaxJoltageOf` function implements our greedy strategy. For each digit position,
we calculate which indices in the original bank are valid candidates (ensuring we leave
enough room for remaining digits), then pick the largest digit from those positions.

The recursion tracks three things: how many digits we've picked so far, the minimum
index we can pick from (to maintain order), and our accumulated result.

\begin{code}
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
\end{code}

Finally, we need to merge our list of digits into an actual number. We reverse the list
(since we built it backwards), then combine digits by multiplying each by the appropriate
power of 10 based on its position.

\begin{code}
-- | Merges digits into number.
-- mergeDigits [1, 2, 3] === 123
mergeDigits :: [Int] -> Int
mergeDigits xs = zip [0..] (reverse xs)
          & foldMap' (\(ix, d) -> Sum (d * 10 ^ ix))
          & getSum

\end{code}
