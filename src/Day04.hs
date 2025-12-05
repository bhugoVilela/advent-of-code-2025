{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Day04 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Linear.V2

type Position = V2 Int
type Grid = HashSet Position

part1 :: String -> Int
part1 = solvePart1 . parse

part2 :: String -> Int
part2 = solvePart2 . parse

solvePart2 :: Grid -> Int
solvePart2 grid = let finalGrid = removeAll grid
                   in length grid - length finalGrid
  where
    removeOnce :: Grid -> Grid
    removeOnce grid = Set.filter (\pos -> getAdjacentRolls grid pos >= 4) grid

    -- | steps until it can't reduce the grid anymore
    removeAll :: Grid -> Grid
    removeAll grid = let !next = removeOnce grid
                      in if Set.size next == Set.size grid
                        then grid
                        else removeAll next

solvePart1 :: Grid -> Int
solvePart1 = freeRollsOfPaper

freeRollsOfPaper :: Grid -> Int
freeRollsOfPaper grid = length . filter (<4) . map (getAdjacentRolls grid) . Set.toList $ grid

getAdjacentRolls :: Grid -> Position -> Int
getAdjacentRolls grid pos = length . filter (`Set.member` grid) . map (pos +) $ adjacentVectors

{-# NOINLINE adjacentVectors #-}
adjacentVectors :: [V2 Int]
adjacentVectors = [V2 a b | a <- [-1..1], b <- [-1..1], a /= 0 || b /=0 ]

parse :: String -> Grid
parse str =
  Set.fromList $
    [ pos
    | (lineIx, line) <- zip [0..] (lines str)
    , (colIx, char) <- zip [0..] line
    , let pos = V2 lineIx colIx
    , char == '@'
    ]
