{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

module Day06 where
import Data.List (unsnoc, transpose)
import Data.Char (isSpace)
import Control.Arrow ((>>>))
import Data.Function ((&))

part1 :: String -> Int
part1 str = 
  let Just (rows, opsLine) = unsnoc . lines $ str
      matrix :: [[Int]]
      matrix = transpose . map (map (read @Int) . words) $ rows
      ops = words opsLine
   in sum . map (uncurry getOp) $ zip ops matrix

part2 :: String -> Int
part2 str = 
  let Just (rows, opsLine) = unsnoc . lines $ str
      numbers = rows 
             & transpose
             & map (strip)
             & splitOn null
             & map (map (read @Int))
      ops = words opsLine
   in sum . map (uncurry getOp) $ zip ops numbers

strip :: String -> String
strip = dropWhile (isSpace) >>> takeWhile (not . isSpace)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn fn xs = let (group, rest) = break fn xs
                    remaining = splitOn fn (drop 1 rest) 
                 in if null group then remaining else group : remaining

getOp :: String -> [Int] -> Int
getOp "+" = sum
getOp "*" = product
getOp _ = error "unreachable"

