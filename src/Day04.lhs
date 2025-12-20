Day 4
=====

Part 1
------

Today's puzzle presents us with a 2D grid containing rolls of paper (marked with `@`)
scattered among empty spaces.

Our task: count how many rolls are **accessible**. A roll is accessible if it has
fewer than 4 neighboring rolls in the 8 adjacent positions (including diagonals).
In other words, rolls that are too crowded (4+ neighbors) are inaccessible.

\begin{code}
{-# LANGUAGE BangPatterns #-}

module Day04 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Linear.V2
\end{code}

[h3] Data Representation

We'll use `V2 Int` from the `linear` package to represent positions, and a `HashSet`
to store the grid.

Since we only care about where the rolls are (not empty spaces), we store just the
positions containing rolls. This gives us efficient O(1) average-case lookups to check
"is there a roll at this position?"

\begin{code}
type Position = V2 Int
type Grid = HashSet Position

parse :: String -> Grid
parse str =
  Set.fromList $
    [ pos
    | (lineIx, line) <- zip [0..] (lines str)
    , (colIx, char) <- zip [0..] line
    , let pos = V2 lineIx colIx
    , char == '@'
    ]
\end{code}

[h3] The Solution

The solution filters the grid to keep only rolls with fewer than 4 neighbors, then
counts them.

\begin{code}
solvePart1 :: Grid -> Int
solvePart1 grid = length . Set.filter ((<4) . getAdjacentRolls grid) $ grid
\end{code}

The `getAdjacentRolls` helper generates all 8 adjacent positions (using vector addition)
and counts how many contain rolls.

\begin{code}
getAdjacentRolls :: Grid -> Position -> Int
getAdjacentRolls grid pos = length . filter (`Set.member` grid) . map (pos +) $ adjacentVectors
  where
    adjacentVectors :: [V2 Int]
    adjacentVectors = [V2 a b | a <- [-1..1], b <- [-1..1], a /= 0 || b /=0 ]

part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

Part 2
------

Part 2 introduces a removal process: we repeatedly remove all accessible rolls (those
with fewer than 4 neighbors) until no more can be removed. The answer is the total
count of removed rolls.

[h3] The Iterative Approach

This is a classic fixed-point iteration. Each round, we:

1. Identify accessible rolls (using our Part 1 logic)
2. Remove them from the grid
3. Repeat until the grid stops changing

The trick: when we remove rolls, previously inaccessible rolls might become accessible
(they now have fewer neighbors). So we keep iterating until we reach a stable state.

The final answer is the difference between the original grid size and the final grid size.

\begin{code}
solvePart2 :: Grid -> Int
solvePart2 grid = let finalGrid = removeAll grid
                   in length grid - length finalGrid
  where
    -- Keep only inaccessible rolls (4+ neighbors)
    removeOnce :: Grid -> Grid
    removeOnce grid = Set.filter (\pos -> getAdjacentRolls grid pos >= 4) grid

    -- Iterate until we reach a fixed point (grid stops changing)
    removeAll :: Grid -> Grid
    removeAll grid = let !next = removeOnce grid
                      in if Set.size next == Set.size grid
                        then grid
                        else removeAll next

part2 :: String -> Int
part2 = solvePart2 . parse
\end{code}
