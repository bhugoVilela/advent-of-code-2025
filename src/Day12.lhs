Day 12
======

Part 1
------

Day 12 brings us a festive packing puzzle! We're playing Santa's logistics coordinator,
trying to figure out which Christmas trees have enough space underneath to hold all their
designated presents.

The input gives us two things: a catalog of present shapes and a list of trees with their
packing requirements.

Each present is defined by its shape on a 2D grid:

```md
0:
 ###
 ##.
 ##.
```

The first line shows the present's index (`0` in this case). The following rows describe
the shape, where `#` represents solid present and `.` represents empty space. This present
occupies 5 cells.

Trees are specified with their dimensions and the presents they need to hold:

```md
4x4: 0 0 0 0 2 0
```

This tree has a 4×4 grid of space underneath (16 cells total), and needs to accommodate:
four copies of present `0`, one copy of present `2`, and one copy of present `0` again.

**Our task**: Determine which trees have sufficient area to fit all their assigned presents.

\begin{code}
module Day12 where
import Data.List (unsnoc)
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Control.Arrow ((>>>))
import System.IO (readFile')
\end{code}

[h3] Modeling the problem

The model is straightforward: we represent each present as a 2D grid of booleans, where
`True` indicates a solid cell. Trees track their dimensions and the list of present indices
they need to hold.

\begin{code}
type Present = [[Bool]]

data Tree =
  Tree { _width :: Int
       , _height :: Int
       , _presents :: [Int]
       } deriving (Show)

type Problem = ([Present], [Tree])
\end{code}

Parsing follows the input structure: we split on double newlines to separate the present
catalog from the tree list. Each present's shape is converted to a grid of booleans by
testing for `'#'` characters. Tree parsing extracts dimensions from the `WxH:` prefix and
reads the space-separated present indices.

\begin{code}
parse :: String -> Problem
parse = splitOn "\n\n"
        >>> unsnoc
        >>> fromMaybe undefined
        >>> bimap (map parsePresent)
                  (map parseTree . lines)
  where
  parsePresent :: String -> Present
  parsePresent = lines >>> tail >>> map (map (== '#'))

  parseTree :: String -> Tree
  parseTree str = let (fst:presents) = splitOn " " str
                      (width, height) = break (== 'x') (init fst)
                  in Tree (read width) (read (drop 1 height)) (map read presents)

\end{code}

[h3] The real problem: 2D bin packing

At first glance, this might look like the classic [knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem)—we
need to fit items into a container. But it's actually harder! This is a **2D bin packing problem**,
where we need to arrange irregularly-shaped presents within a rectangular space.

The general version of this problem is NP-complete. A proper solution would need to:

1. Try different rotations of each present (if rotation is allowed)
2. Consider all possible positions for placing each present
3. Ensure presents don't overlap with each other
4. Backtrack when a configuration doesn't work

This could involve sophisticated algorithms like genetic algorithms, simulated annealing,
or branch-and-bound search. For large inputs, even approximate solutions are challenging!

[h3] A pragmatic shortcut

Here's where things get interesting. After examining the actual puzzle input, I noticed
something: **for every tree in the input, a simple area check is sufficient**. That is,
if the sum of all present areas is less than or equal to the tree's total area, then
those presents will actually fit when properly arranged.

This suggests the puzzle author deliberately crafted the input to avoid the NP-complete
complexity. Perhaps the presents are designed to pack efficiently, or the tree spaces are
generous enough that optimal packing isn't required. Either way, we can solve Part 1 with
a straightforward area comparison!

The solution filters trees by checking if their total area can accommodate all assigned
presents. We pre-compute each present's area (counting `True` cells in its grid), then
for each tree, sum up the areas of its required presents and compare against the tree's
dimensions.

\begin{code}
solvePart1 :: Problem -> Int
solvePart1 (presents, trees) = length $ filter (fitsAllPresents) trees
  where
  fitsAllPresents :: Tree -> Bool
  fitsAllPresents (Tree width height presents') =
    let totalPresentsArea = foldl' (\acc (ix, count) -> acc + (count * (presentsArea !! ix))) 0 (zip [0..] presents')
        totalArea = width * height
     in totalPresentsArea <= (totalArea)

  presentsArea = map (length . filter id . concat) presents

part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

This elegant simplification turns an intractable problem into an O(n) solution. Sometimes
the best algorithm is recognizing when you don't need one!

Part 2
------

There is no Part 2, part 2 is the stars we collected along the way (:

I hope this has been useful to you, if it was please consider leaving a star in the repo.

---
Merry Christmas, <br>
Hugo Vilela
<br>

