{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

module Day07 where
import Data.Function ((&), on)
import Data.List (find, sortBy, sort, uncons)
import Data.HashMap.Lazy (HashMap, (!?))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Control.Monad (guard)

data Instructions = Instructions {
  _start :: (Int, Int), -- where the beam starts
  _tachyons :: HashMap Int [Int]  -- maps column to list of y coordinate tachyons
} deriving (Show)

part1 :: String -> Int
part1 = solvePart1 . parse

part2 :: String -> Int
part2 = solvePart2 . parse

solvePart1 :: Instructions -> Int
solvePart1 (Instructions start tachyons) = Set.size $ go mempty start tachyons
  where
    go :: HashSet (Int, Int) -> (Int, Int) -> HashMap Int [Int] -> HashSet (Int, Int)
    go cache (x, y) tachyons =
      let match = do 
            col <- tachyons !? x
            (head, rest) <- uncons $ dropWhile (< y) col
            guard $ not $ (x, head) `Set.member ` cache
            let newMap = Map.insert x rest tachyons 
            Just (head, newMap)
       in case match of
            Just (newY, newMap) -> 
              let newCache = Set.insert (x, newY) cache
                  lhs = go newCache (x - 1, newY) newMap
                  rhs = go lhs (x + 1, newY) newMap
               in Set.union lhs rhs
            Nothing -> cache

solvePart2 :: Instructions -> Int
solvePart2 (Instructions start tachyons) = snd $ go mempty start tachyons
  where
    go :: HashMap (Int, Int) Int -> (Int, Int) -> HashMap Int [Int] -> (HashMap (Int, Int) Int, Int)
    go cache (x, y) tachyons =
      let match = do 
            col <- tachyons !? x
            (head, rest) <- uncons $ dropWhile (< y) col
            let newMap = Map.insert x rest tachyons 
            Just (head, newMap, cache Map.!? (x, head))
       in case match of
            Just (newY, newMap, count) -> 
              case count of
                Just n -> (cache, n)
                Nothing -> let (leftCache, nl) = go cache (x - 1, newY) newMap
                               (rightCache, nr) = go leftCache (x + 1, newY) newMap
                               newCache = Map.insert (x, newY) (nl + nr) $
                                 Map.union leftCache rightCache
                           in (newCache, nl + nr)
            Nothing -> (Map.empty, 1)

parse :: String -> Instructions
parse str = let chars :: [(Char, (Int, Int))]
                chars = [ (c, (x, y)) 
                        | (y, line) <- zip [0..] (lines str)
                        , (x, c)    <- zip [0..] line
                        , c /= '.'
                        ]
                Just (_, start) = find (\(c, _) -> c == 'S') chars
                tachyons = chars 
                           & filter (\(c, _) -> c == '^')
                           & map snd
                           & sortBy (compare `on` fst)
                           & groupBy fst
                           & map (\col -> (fst $ head col, map snd col))
                           & Map.fromList
                           & fmap (sort)
             in Instructions start tachyons 

groupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy fn [] = []
groupBy fn (x:xs) = let (group, rest) = span (\a -> fn a == fn x) xs
                     in (x:group) : groupBy fn rest

