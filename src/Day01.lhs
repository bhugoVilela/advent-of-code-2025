Day 1
=====

Part 1
------

Today's puzzle involves a combination lock with a rotating dial. 
We're given a series of left (L) and right (R) rotations, and we need to count how many times the dial lands exactly on position 0 after completing each rotation.

The dial has 100 positions (0-99) and starts at position 50.

\begin{code}

\end{code}

First, a few ceremonies...

\begin{code}
{-# LANGUAGE TypeApplications #-}
module Day01 where
\end{code}

[h3] Parsing the Input

I like to start by establishing a model and parsing the input.

A `List` of `Integer`s is a natural way to model rotations. We'll use **positive integers** for right rotations and **negative integers** for left rotations. This makes the math simpler later—we can just add the rotation value to our current position.

\begin{code}
parse :: String -> [Int]
parse = map parseLine . lines
  where
    parseLine :: String -> Int
    parseLine ('R':rest) = read @Int rest
    parseLine ('L':rest) = negate $ read @Int rest
    parseLine _ = undefined

\end{code}

[h3] Solving Part 1

To solve this, we'll fold over the list of rotations while maintaining a tuple of `(count, currentRotation)`:

- `count`: the number of times we've landed exactly on 0
- `currentRotation`: our current position on the dial

The key insight: after each rotation, we check if our new position is divisible by 100 by using modulo (meaning we're at position 0).

\begin{code}
solvePart1 :: [Int] -> Int
solvePart1 = fst . foldl' rotate (0, 50)
  where
  rotate :: (Int, Int) -> Int -> (Int, Int)
  rotate (count, currentRotation) rotation =
    let newRotation = currentRotation + rotation
        isZero = newRotation `mod` 100 == 0
     in (count + if isZero then 1 else 0, newRotation)
\end{code}

All that's left is to compose parsing with solving:

\begin{code}
part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

Part 2
------

Part 2 increases the difficulty: now we need to track how many times the indicator **passes through** position 0, including positions crossed during a rotation (not just where it lands).

For example, if we're at position 80 and rotate right by 30, we pass through position 0 once during that rotation (80 → 90 → 100/0 → 10).

[h3] The Algorithm

The base algorithm remains a fold over the rotation list, but now we need a more sophisticated way to count zero crossings:

\begin{code}
solvePart2 :: [Int] -> Int
solvePart2 rotations = fst $ foldl' go (0, 50) rotations
  where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (count, currentRotation) rotation =
    let newRotation = currentRotation + rotation
        clicks = numberOfClicks currentRotation rotation
     in (count + clicks, newRotation)
\end{code}

[h3] Counting Zero Crossings

The `numberOfClicks` function calculates how many times we pass through zero during a single rotation.

The logic breaks down as follows:

1. **Normalize the current position** to be in range [0, 99]
2. **Calculate complete rotations**: Dividing the rotation amount by 100 gives us how many full loops around the dial we make (`totalRotations`)
3. **Check the remainder**: After the complete rotations, does the remainder push us through 0?
   - **Right rotation**: We pass through 0 if `currentPos + remainder >= 100`
   - **Left rotation**: We pass through 0 if we're not already at 0 and `currentPos - remainder <= 0`

\begin{code}
numberOfClicks :: Int -> Int -> Int
numberOfClicks pos rotation =
  let actualPos = normalizeRotation pos
      (totalRotations, remainder) = quotRem (abs rotation) 100
      remainderClicks = if rotation > 0
        then actualPos + remainder >= 100
        else actualPos /= 0 && actualPos - remainder <= 0
  in totalRotations + if remainderClicks then 1 else 0
\end{code}

[h3] Handling Negative Positions

One edge case: Haskell's `mod` operator doesn't wrap negative numbers the way we need for a circular dial. We need a custom normalization function:
This ensures that `-10` becomes `90`, `-110` becomes `90`, etc.

\begin{code}
normalizeRotation :: Int -> Int
normalizeRotation n
  | n >= 0    = n `mod` 100
  | otherwise = (100 - (abs n) `mod` 100) `mod` 100
\end{code}

[h3] Final Solution

And TA-DA!

\begin{code}
part2 :: String -> Int
part2 = solvePart2 . parse
\end{code}
