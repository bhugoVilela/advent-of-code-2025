Day 6
=====

Part 1
------

Today's puzzle gives us a grid of numbers followed by a line of operations.

The twist? We need to read the grid **vertically** (column by column) rather than
horizontally. Each column of numbers gets paired with an operation from the last line,
and we apply that operation to get a result. The final answer is the sum of all
column results.

For example, if a column is `[2, 3, 5]` and the operation is `+`, we get `10`.
If the operation is `*`, we get `30`.

\begin{code}
{-# LANGUAGE TypeApplications #-}
module Day06 where
import Data.List (unsnoc, transpose)
import Data.Char (isSpace)
import Control.Arrow ((>>>))
import Data.Function ((&))
\end{code}

[h3] Part 1 Solution

The algorithm is straightforward:

1. Split the input into rows and the final operations line
2. Parse each row as space-separated numbers
3. **Transpose** to get columns instead of rows
4. Zip operations with columns and apply each operation
5. Sum the results

\begin{code}
part1 :: String -> Int
part1 str =
  let Just (rows, opsLine) = unsnoc . lines $ str
      matrix :: [[Int]]
      matrix = transpose . map (map (read @Int) . words) $ rows
      ops = words opsLine
   in sum $ zipWith getOp ops matrix
\end{code}

[h3] Operations

We support two operations: sum (`+`) and product (`*`).

\begin{code}
getOp :: String -> [Int] -> Int
getOp "+" = sum
getOp "*" = product
getOp _ = error "unreachable"
\end{code}

Part 2
------

Part 2 changes the parsing rules. Now the grid can contain **empty spaces** within
columns, and we need to treat sequences of numbers separated by spaces as distinct
groups within each column.

For example, a column might look like:
```
1
2

3
4
```

This represents two groups: `[1, 2]` and `[3, 4]`. We split on empty spaces, apply
operations to each group separately, then sum everything.

[h3] The Solution

The key difference is in how we process the transposed data:

1. Strip whitespace from each cell
2. Split on empty strings to get groups
3. Parse each group as numbers
4. Apply operations as before

\begin{code}
part2 :: String -> Int
part2 str =
  let Just (rows, opsLine) = unsnoc . lines $ str
      numbers = rows
             & transpose
             & map strip
             & splitOn null
             & map (map (read @Int))
      ops = words opsLine
   in sum $ zipWith getOp ops numbers
\end{code}

[h3] Helper Functions

We need helpers to strip whitespace and split on a predicate.

\begin{code}
strip :: String -> String
strip = dropWhile isSpace >>> takeWhile (not . isSpace)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn fn xs = let (group, rest) = break fn xs
                    remaining = splitOn fn (drop 1 rest)
                 in if null group then remaining else group : remaining
\end{code}
