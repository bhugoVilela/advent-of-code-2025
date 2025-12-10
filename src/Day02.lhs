Day 2
=====
Part 1
------

Today's puzzle gives us a list of numeric ranges:

```md
(90 - 10000)
(57 - 82345)
```

Our task: find all "invalid IDs" within each range and sum them together.

An ID is invalid if its digits can be split into two equal halves. For example:

- `1010` splits into `10` and `10` ✓ invalid
- `123123` splits into `123` and `123` ✓ invalid
- `88` splits into `8` and `8` ✓ invalid
- `123` has odd length ✗ valid
- `1234` splits into `12` and `34` ✗ valid

\begin{code}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import qualified Data.Text as T
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List.Split (chunksOf)
\end{code}

[h3] Parsing

The input is comma-separated ranges, which we'll model as pairs of integers.

\begin{code}
type Range = (Int, Int)

parse :: String -> [Range]
parse = T.pack >>> T.splitOn "," >>> map parseRange
  where
    parseRange str = let [a, b] = T.splitOn "-" str
                      in (read @Int $ T.unpack a, read @Int $ T.unpack b)
\end{code}

[h3] The Solution

The algorithm is straightforward: for each range, check every number to see if it's
invalid, sum those that are, then sum across all ranges.

The `isInvalid` function checks if a number has even digit length and whether its
first half equals its second half.

\begin{code}
part1 :: String -> Int
part1 = parse >>> map invalidIdsInRange >>> sum
  where
    invalidIdsInRange :: Range -> Int
    invalidIdsInRange (a, b) = sum $ filter isInvalid $ [a..b]
    isInvalid :: Int -> Bool
    isInvalid n = let str = show n
                      [fst, snd] = chunksOf (length str `div` 2) str
                   in even (length str) && fst == snd

\end{code}

Part 2
------

Part 2 generalizes the pattern: now an ID is invalid if it consists of any repeating
segment, not just two halves. For example:

- `111` → `1` repeated 3 times ✓ invalid
- `123123123` → `123` repeated 3 times ✓ invalid
- `88` → `8` repeated 2 times ✓ invalid
- `1234` → no repeating pattern ✗ valid

[h3] The Key Insight

We need to find all possible chunk sizes that could divide the number evenly, then
check if chunking by that size produces identical chunks.

For a number with length 6, we check chunk sizes 1, 2, and 3 (divisors of 6). If
any chunking produces all-equal chunks, the number is invalid.

The implementation changes only the `isInvalid` function—everything else stays the same.

\begin{code}
part2 :: String -> Int
part2 = parse >>> map invalidIdsInRange >>> sum
  where
  invalidIdsInRange :: Range -> Int
  invalidIdsInRange (a, b) = sum $ filter isInvalid $ [a..b]
  isInvalid :: Int -> Bool
  isInvalid n = let str = show n
                    divs = [1..length str `div` 2] & filter (\it -> length str `mod` it == 0)
                 in any (allEqual . flip chunksOf str) divs

allEqual :: (Eq a) => [a] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual [] = True
\end{code}
