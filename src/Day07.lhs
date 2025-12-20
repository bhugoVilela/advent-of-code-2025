Day 7
=====

Part 1
------

Today's puzzle involves a beam of light traveling through a grid containing tachyons
(represented by `^` symbols). The beam starts at position `S` and travels upward
through columns.

When the beam hits a tachyon, something interesting happens: it **splits** into two
beams that move horizontally (one left, one right), then continue upward from their
new positions.

Our task: count how many unique tachyons the beam(s) will hit.

\begin{code}
module Day07 where
import Data.Function ((&), on)
import Data.List (find, sortBy, sort, uncons)
import Data.HashMap.Lazy (HashMap, (!?))
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Control.Monad (guard)
\end{code}

[h3] Data Model

We need to track:

- Where the beam starts (`S` position)
- Where all the tachyons are (organized by column for efficient lookup)

\begin{code}
data Instructions = Instructions {
  _start :: (Int, Int),
  _tachyons :: HashMap Int [Int]  -- maps column x to list of y coordinates
} deriving (Show)
\end{code}

[h3] Parsing

We scan the grid for special characters: `S` marks the start position, and `^` marks
tachyons. We organize tachyons by column and sort their y-coordinates for efficient
upward traversal.

\begin{code}
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
                           & fmap sort
             in Instructions start tachyons
\end{code}

[h3] Helper: Grouping

A simple grouping function that clusters consecutive elements with the same key.

\begin{code}
groupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy _ [] = []
groupBy fn (x:xs) = let (group, rest) = span (\a -> fn a == fn x) xs
                     in (x:group) : groupBy fn rest
\end{code}

[h3] The Algorithm

The beam simulation is a recursive traversal:

1. From the current position `(x, y)`, look up the column and find the next tachyon
   above the current y-coordinate
2. If we find one and haven't visited it yet:
   - Mark it as visited
   - Remove it from the available tachyons
   - Split: recursively simulate beams going left `(x-1, newY)` and right `(x+1, newY)`
3. If no tachyon found or already visited, return the current visited set

The union of all visited tachyons gives us the answer.

\begin{code}
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

part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

Part 2
------

Part 2 asks: how many distinct **paths** does the beam take to reach the end?

Instead of just counting visited tachyons, we now count all the different ways the
beam can split and travel through the grid. This is a classic path-counting problem
that benefits from **memoization**—if we reach the same tachyon from the same direction
multiple times, the number of paths from there is always the same.

[h3] The Memoized Solution

The algorithm is similar to Part 1, but now:

- We track a cache of `(position -> path_count)` instead of a set
- When we reach a position we've seen before, return the cached count
- When we reach the end (no more tachyons), that's 1 complete path
- The total count is the sum of paths going left and right at each split

\begin{code}
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
                Just n -> (cache, n)  -- Cache hit!
                Nothing -> let (leftCache, nl) = go cache (x - 1, newY) newMap
                               (rightCache, nr) = go leftCache (x + 1, newY) newMap
                               newCache = Map.insert (x, newY) (nl + nr) $
                                 Map.union leftCache rightCache
                           in (newCache, nl + nr)
            Nothing -> (Map.empty, 1)  -- Reached the end, count as 1 path

part2 :: String -> Int
part2 = solvePart2 . parse
\end{code}
