Day 5
=====

Part 1
------

Today's puzzle gives us a database containing two pieces of information:

1. A list of valid ranges (e.g., `100-500`, `600-1000`)
2. A list of ID numbers to check

Our task for Part 1: count how many of the given IDs fall within at least one of
the valid ranges. An ID is "fresh" if it's covered by any range.

\begin{code}
module Day05 where

import Data.List (sortBy)
import Data.Function (on)
\end{code}

[h3] Data Model

We'll represent ranges as pairs of integers and bundle everything into a `Database`
type for clean organization.

\begin{code}
type Range = (Int, Int)

data Database = Database
  { ranges :: [Range]
  , ids :: [Int]
  } deriving (Show)
\end{code}

[h3] Parsing

The input format has ranges first (one per line), then a blank line, then a list of IDs.
We split on the blank line and parse each section accordingly.

\begin{code}
parse :: String -> Database
parse str =
  let rows = lines str
      (rangeLines, idLines) = break null rows
   in Database
        (map parseRange rangeLines)
        (map read $ drop 1 idLines)
  where
  parseRange :: String -> Range
  parseRange str = let (fst', snd') = break (== '-') str
                    in (read fst', read . drop 1 $ snd')
\end{code}

[h3] The Solution

Part 1 is straightforward: for each ID, check if any range contains it. Count the
IDs that pass this test.

The `contains` helper checks if a value falls within a range's bounds (inclusive).

\begin{code}
solvePart1 :: Database -> Int
solvePart1 (Database ranges ids) = length $ filter isFresh ids
  where
  isFresh :: Int -> Bool
  isFresh id = any (contains id) ranges

  contains :: Int -> Range -> Bool
  contains id (min, max) = id >= min && id <= max

part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

Part 2
------

Part 2 asks a different question: what's the total count of unique integers covered
by all the ranges combined?

For example, if we have ranges `1-5` and `3-8`, the unique integers covered are
`[1,2,3,4,5,6,7,8]`, giving us a count of 8 (not 11, which would be if we counted
overlaps twice).

[h3] The Sorting Insight

The key insight: if we sort the ranges by their start position, we know that each
subsequent range never starts before any previous range. This makes merging overlaps
much simpler.

As we fold through the sorted ranges, we track:
- The previous range's maximum value
- The running count of unique integers covered

For each new range, we calculate how many *new* integers it contributes by ensuring
we don't double-count overlaps with the previous range.

\begin{code}
solvePart2 :: Database -> Int
solvePart2 = snd . foldl' addRange empty . sortBy (compare `on` fst) . ranges
  where
  countRange (min, max) = max - min + 1

  addRange :: (Range, Int) -> Range -> (Range, Int)
  addRange ((_, prevMax), count) (nextMin, nextMax) =
    let newRange = ( max nextMin (prevMax + 1), max nextMax prevMax)
     in (newRange, count + countRange newRange)

  empty :: (Range, Int)
  empty = ((undefined, -2), 0)

part2 :: String -> Int
part2 = solvePart2 . parse
\end{code}
