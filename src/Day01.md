# Day 1

## Part 1
For today's problem we have a list of left/right rotations to open a lock 
and need to count the number of times the dial rests at 0 after each rotation

```haskell

```

First, a few ceremonies...

```haskell
{-# LANGUAGE TypeApplications #-}
module Day01 where
```

I like to start with establishing a model and parsing the input.<br/><br/>
A List of Integers is a good way to model rotations, negative ints being left rotations.

```haskell
parse :: String -> [Int]
parse = map parseLine . lines
  where
    parseLine :: String -> Int
    parseLine ('R':rest) = read @Int rest
    parseLine ('L':rest) = negate $ read @Int rest
    parseLine _ = undefined

```

And to solve, we want to fold over the list of rotations keeping track of `(count, currentRotation)`
where `count` is the number of times we land at 0
<br/> <br/>
This is where the magic happens, we add the rotation we're folding into the `currentRotation`
and then use modulo to check if the `newRotation` is at zero

```haskell
solvePart1 :: [Int] -> Int
solvePart1 = fst . foldl' rotate (0, 50)
  where
  rotate :: (Int, Int) -> Int -> (Int, Int)
  rotate (count, currentRotation) rotation = 
    let newRotation = currentRotation  + rotation
        isZero = newRotation `mod` 100 == 0
     in (count + if isZero then 1 else 0, newRotation)
```

All that's left is to define the function that solves after parsing.

```haskell
part1 :: String -> Int
part1 = solvePart1 . parse
```

## Part 2

Part2 raises things up a notch, now we want to track the number of times the
indicator goes through 0 (counting mid rotations as well)

The base algorithm is the same, a fold over the list, the difference is in how we count the clicks

```haskell
solvePart2 :: [Int] -> Int
solvePart2 rotations = fst $ foldl' go (0, 50) rotations
  where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (count, currentRotation) rotation = 
    let newRotation = currentRotation  + rotation
        clicks = numberOfClicks currentRotation rotation
     in (count + clicks, newRotation)
```

We want to count how many times the indicator went through zero when starting at `pos`
and rotating `rotation`.<br/>
We divide the rotation by 100 to get the number of rotations performed 
+ the remainder which may guide us through 0 once more

```haskell
numberOfClicks :: Int -> Int -> Int
numberOfClicks pos rotation = 
  let actualPos = normalizeRotation pos
      (totalRotations, remainder) = quotRem (abs rotation) 100
      remainderClicks = if rotation > 0 
        then actualPos + remainder >= 100
        else actualPos /= 0 && actualPos - remainder <= 0
  in totalRotations + if remainderClicks then 1 else 0
```

one edge case here as modulo doesn't wrap around negative numbers as one might expect

```haskell
normalizeRotation :: Int -> Int
normalizeRotation n
  | n >= 0    = n `mod` 100
  | otherwise = (100 - (abs n) `mod` 100) `mod` 100
```

and TA-DA!

```haskell
part2 :: String -> Int
part2 = solvePart2 . parse
````

