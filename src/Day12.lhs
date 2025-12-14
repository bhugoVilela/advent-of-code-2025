Day 12
======

Part 1
------

Today we're given a list of presents each looking like this:

```md
0:
 ###
 ##.
 ##.
```

the first line has the index of the present and what follows
is the shape of the present where '#' is part of the present and '.'
is empty space.

We're also given a list of trees (or rather, the space available for presents beneath it).
with a width a height and the number of presents.

we're asked which trees can hold all the presents in their list.

```md
4x4: 0 0 0 0 2 0
```

in this case it's 4x4 and has 2 presents with the index 2 shape.

\begin{code}
module Day12 where
import Data.List (unsnoc)
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Control.Arrow ((>>>))
import System.IO (readFile')
\end{code}

The model is straighforward

\begin{code}
type Present = [[Bool]]

data Tree = 
  Tree { _width :: Int
       , _height :: Int
       , _presents :: [Int]
       } deriving (Show)

type Problem = ([Present], [Tree])
\end{code}

And so is the parser

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

Well, while this is the knapsack problem.

Unfortunately, the input for this day was poorly done.

With the input given it suffices to check whether a tree has enough area
for all the presents. (treeArea >= sumOfPresentsArea)

One day I'll come back and finish this one properly.

To be fair, this problem is NP complete. So perhaps the simplified input was on purpose

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

