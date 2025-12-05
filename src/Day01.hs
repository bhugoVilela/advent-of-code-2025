{-# LANGUAGE TypeApplications #-}
module Day01 where

part1 :: String -> Int
part1 = solvePart1 . parse

part2 :: String -> Int
part2 = solvePart2 . parse

parse :: String -> [Int]
parse = map parseLine . lines
  where
    parseLine :: String -> Int
    parseLine ('R':rest) = read @Int rest
    parseLine ('L':rest) = negate $ read @Int rest
    parseLine _ = undefined

solvePart1 :: [Int] -> Int
solvePart1 rotations = fst $ foldl' go (0, 50) rotations
  where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (count, currentRotation) rotation = 
    let newRotation = currentRotation  + rotation
        isZero = newRotation `mod` 100 == 0
     in (count + if isZero then 1 else 0, newRotation)

solvePart2 :: [Int] -> Int
solvePart2 rotations = fst $ foldl' go (0, 50) rotations
  where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (count, currentRotation) rotation = 
    let newRotation = currentRotation  + rotation
        clicks = numberOfClicks currentRotation rotation
     in (count + clicks, newRotation)

numberOfClicks :: Int -> Int -> Int
numberOfClicks pos rotation = 
  let actualPos = normalizeRotation pos
      (totalRotations, remainder) = quotRem (abs rotation) 100
      remainderClicks = if rotation > 0 
        then actualPos + remainder >= 100
        else actualPos /= 0 && actualPos - remainder <= 0
  in totalRotations  + if remainderClicks then 1 else 0

normalizeRotation :: Int -> Int
normalizeRotation n
  | n >= 0    = n `mod` 100
  | otherwise = (100 - (abs n) `mod` 100) `mod` 100
