# Day 2

## Part 1

Today's puzzle gives us a list of integer ranges (N-M) and asks us to count "invalid IDs" within those ranges. An ID is invalid if it's a number where the first half equals the second half (e.g., 123123, 7777, 9898).

The key challenge: some ranges can be enormous (like 101-9999999999), so we can't just check every number individually.

A few ceremonies:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import qualified Data.Text as T
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List (sort, nub)
```

### Parsing the Input

As always, we start with parsing. The input is a comma-separated list of ranges (e.g., "101-999,5000-10000").

A tuple is a natural fit for representing a range. I chose to parse into `(Text, Text)` rather than `(Int, Int)` because it makes it easier to work with the number lengths and manipulate the digits later.

```haskell
type Range a = (a, a)

parse :: T.Text -> [Range T.Text]
parse = T.splitOn "," >>> map parseRange
  where
    parseRange str = let [a, b] = T.splitOn "-" str
                      in (a, b)
```

### The Clever Trick: Think by Length, Not by Value

Okay, time to think this through. Let's say we have the range `(101-9999999999)`. That's billions of numbers to check individually—way too slow!

**Key insight**: Instead of iterating through every number, we can group them by their *length* (number of digits).

#### Why This Works

1. **Odd-length numbers can never be invalid**: You can't split 3 digits evenly (e.g., 100-999 are all valid)
2. **For even-length numbers, we only need to check one half**:
   - For 4-digit numbers like `9898`, we just need the 2-digit number `98` and duplicate it
   - For 6-digit numbers like `123123`, we just need `123` and duplicate it

This reduces the search space dramatically! Instead of checking billions of numbers, we only generate the small set that could possibly be invalid.

#### The Algorithm

**`invalidIdsInRange`** finds all invalid IDs within a given range by:

1. Getting all even number-lengths within the range (e.g., for range 10-100000, we check lengths 2, 4, 6)
2. For each length, generating all possible invalid IDs of that length
3. Filtering to keep only those within the actual range bounds

**`invalidIdsByNumLength`** is an infinite list indexed by number length. At each index, it contains all invalid IDs of that length. For example:

- Index 2: `[11, 22, 33, ..., 99]`
- Index 6: `[100100, 101101, ..., 999999]`

Given a certain length (say 4), instead of going through each number and checking if it's invalid,
we generate all length 2 numbers (ie. [10-99]) and duplicate them (ie. 10 -> 1010) and this is how we get
all invalid ids for that length.

> **A word on infinite lists in haskell**
>
> Infinite lists are extremely useful in haskell as a way of memoisation for two reasons.
>
> 1. Because haskell is lazy it will only compute the values that we ask from the list.
> 2. The second time we access a value from the list its already precomputed and is passed by reference
>
> If `invalidIdsByNumLength` was a function it would recompute every time it's invoked.
> this way we're able to reuse work that's already been done.

```haskell
part1 :: String -> Int
part1 = T.pack
      >>> parse
      >>> concatMap invalidIdsInRange
      >>> sum
  where
    invalidIdsInRange :: Range T.Text -> [Int]
    invalidIdsInRange (a, b) = [T.length a .. T.length b]
                               & filter even                          -- Only even-length numbers (odd lengths can't be mirrored)
                               & concatMap (invalidIdsByNumLength !!) -- Get all invalid IDs of these lengths
                               & dropWhile (< textRead @Int a)        -- Drop IDs below range start
                               & takeWhile (<= textRead @Int b)       -- Take only IDs within range end

    -- Infinite list of all possible invalid IDs by length
    invalidIdsByNumLength :: [[Int]]
    invalidIdsByNumLength = [0..]
      & map (
          -- For each length, generate the first half and duplicate it
          -- e.g., for length 4: take all 2-digit numbers [10..99] and duplicate to [1010..9999]
         map (read @Int) . map (\a -> a <> a) . getNDigits . (`div` 2)
      )
```

## Part 2

Part 2 raises the stakes! Now an invalid ID is **any number that has a repeating pattern** (a substring repeated 2 or more times).

### Examples of Invalid IDs

- `111` (the digit "1" repeated 3 times)
- `10101010` (the substring "10" repeated 4 times)
- `125125` (the substring "125" repeated 2 times)
- `123123123` (the substring "123" repeated 3 times)

### The Approach: Find All Divisors

My approach builds on Part 1: we still process numbers by their length. But now, for each length, we need to find **all possible repeating patterns**.

The key insight: a repeating pattern of length N can only exist in a number of length L if N divides L evenly.

For a 6-digit number, the divisors are 1, 2, and 3:

| Divisor | Pattern | Example |
|---------|---------|---------|
| 1 | `XXXXXX` | `111111` (repeat "1" six times) |
| 2 | `XYXYXY` | `121212` (repeat "12" three times) |
| 3 | `XYZXYZ` | `123123` (repeat "123" twice) |

Note: Divisor 6 doesn't count because we need **at least 2 repetitions**.

### The Algorithm

**`invalidIdsByNumLength`** remains virtually the same

**`getInvalidIds`** does the heavy lifting for a single length:

1. Find all valid divisors (prefix lengths that divide evenly)
2. For each divisor, generate all possible numbers of that length
3. Repeat each number the appropriate number of times to fill the full length
4. Combine, sort, and deduplicate all results to get a clean `[[Int]]`

### Helper Functions

- **`getNDigits n`**: Returns all n-digit numbers as strings (e.g., `getNDigits 2 = ["10", "11", ..., "99"]`)
- **`numLength`**: Returns the number of digits in a number (e.g., `numLength 100 = 3`)
- **`textRead`**: Convenience function to read from Text

This approach efficiently generates exactly the invalid IDs we need without checking every possible 6-digit number!

```haskell
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

    -- List of invalid IDs by length of number
    invalidIdsByNumLength :: [[Int]]
    invalidIdsByNumLength = [0..] & map getInvalidIds
      where getInvalidIds numLen =
              let prefixLengths = filter (\prefixLen -> numLen `rem` prefixLen == 0) [1..(numLen - 1)]
                  -- How many times we need to repeat a prefix to reach numLen
                  prefixRepeats prefixLen = numLen `quot` prefixLen
                  invalidIds = prefixLengths
                    -- Generate all invalid IDs by repeating each possible prefix
                    & map (\prefixLen -> map (read @Int
                            . concat
                            . replicate (prefixRepeats prefixLen))
                        $ getNDigits prefixLen)
               in sort . nub . concat $ invalidIds

    numbersByLength :: [[Int]]
    numbersByLength = [0..] & map (map (read @Int) . getNDigits)

-- | Get the number of digits in a number
-- e.g. numLength 100 = 3
numLength :: Int -> Int
numLength = length . show

-- | Returns all numbers with n digits in ascending order
-- e.g. getNDigits 2 = ["10", "11", ..., "99"]
getNDigits :: Int -> [[Char]]
getNDigits numChars =
  let first = ['1'..'9']      -- First digit can't be 0
      other = ['0'..'9']      -- Other digits can be anything
      all = first : replicate (numChars - 1) other
   in map reverse $ foldl' go ([""]) all
   where
    go :: [String] -> String -> [String]
    go acc digits = flip concatMap acc $ \str -> map (:str) digits

textRead :: (Read a) => T.Text -> a
textRead = read . T.unpack
```
