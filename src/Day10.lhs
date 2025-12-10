Day 10
======

Part 1
------

Today's puzzle presents us with a collection of lights, each composed of multiple LEDs.
Our task: figure out the minimum number of button presses needed to configure each light
to match a target pattern.

Each light comes with three pieces of information:

- **Target configuration**: a string like `[.##.]` where `#` represents an LED that should
  be ON and `.` represents one that should be OFF
- **Buttons**: each button, when pressed, toggles specific LEDs (e.g., `(1,3)` toggles
  LEDs at indices 1 and 3)
- **Joltages**: power requirements we can safely ignore for Part 1

For example, in `[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}`:
- We want LEDs at positions 1 and 2 to be ON, the rest OFF
- We have six buttons available, each toggling different LED combinations
- The joltages `{3,5,4,7}` don't matter yet

This is essentially a search problem: starting from all LEDs off, find the shortest sequence
of button presses to reach the target configuration

\begin{code}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module Day10 where
import Data.List (uncons)
import Data.Function ((&))
import Data.Bits ((.<<.), (.|.))
import Data.List.Split (splitOn)
import Control.Arrow ((>>>))
import System.IO (readFile')
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.IntSet as Set
import Data.IntSet (IntSet)
import Debug.Trace (traceShowId)
\end{code}

[h3] Modeling the problem

We'll represent each light with a simple record type. The key insight is that we only
care about *which* LEDs are on, not their order, so `IntSet` is perfect for tracking
configurations efficiently.

\begin{code}
data Light = Light {
  _target :: IntSet,
  _buttons :: [[Int]],
  _joltages :: [Int]
} deriving (Show)
\end{code}

Parsing this format is a bit fiddly—we need to strip brackets, parentheses, and curly braces,
then split on commas and whitespace. Not the most elegant code I've written, but it gets
the job done without pulling in heavy parsing libraries.

**Reminder to Self**: find better combinators for parsing. (Although I'd like to keep my solutions parsec and regex free)

\begin{code}
parse :: String -> [Light]
parse = map (parseLine . words) . lines
  where
  parseLine :: [String] -> Light
  parseLine words = 
    let Just (light, rest) = uncons words
        (buttons, [joltages]) = break ((== '{') . head) rest
     in Light (parseLight light) (map parseButton buttons) (parseJoltages joltages)

  parseLight :: String -> IntSet
  parseLight str = filter (\c -> c /= '[' && c /= ']') str
                 & zip [0..]
                 & filter ((== '#') . snd)
                 & map (fst)
                 & Set.fromList

  parseButton :: String -> [Int]
  parseButton = filter (\c -> c /= '(' && c /= ')') 
              >>> splitOn ","
              >>> map (read @Int)


  parseJoltages :: String -> [Int]
  parseJoltages = filter (\c -> c /= '{' && c /= '}') >>> splitOn "," >>> map (read @Int)
\end{code}

[h3] Finding the shortest path

When we want the *minimum* number of steps to reach a goal, breadth-first search (BFS)
is our go-to algorithm. It explores all configurations reachable in N steps before
trying any configuration that takes N+1 steps—guaranteeing we find the shortest path.

Here's the game plan:
1. Start with all LEDs off (empty `IntSet`)
2. For each configuration we're exploring, try pressing every button
3. Track which configurations we've seen before to avoid cycles
4. Stop when we reach the target configuration

The `breadth` function does the heavy lifting:
- `states`: configurations we're currently exploring
- `cache`: a map recording how many steps it took to reach each configuration
- `steps`: current depth in our search

Each iteration generates new configurations by clicking every possible button on every
current state, filters out ones we've seen before, and continues until we hit the target

The helper functions are straightforward: `clickButton` applies a button press to a
configuration by toggling each LED the button affects, and `toggleLight` flips a
single LED on or off.

\begin{code}
findMinButtonPresses :: Light -> Int
findMinButtonPresses (Light target buttons _) = 
  let (cache, _) = breadth 0 (Map.singleton Set.empty 0)[ Set.empty ]
   in cache Map.! target
  where
  breadth :: Int -> HashMap IntSet Int -> [IntSet] -> (HashMap IntSet Int, [IntSet])
  breadth steps cache states = 
    let !b = concatMap (\s -> 
              filter (not . (flip Map.member cache)) 
              . map (clickButton s) 
              $ buttons ) 
              states
        !updatedCache = foldl' (\acc k -> Map.insert k (steps + 1) acc) cache b
        !found = Map.member target updatedCache
     in if null b then error "unreachable" else if found 
      then (updatedCache, b)
      else breadth (steps + 1) updatedCache b

  clickButton :: IntSet -> [Int] -> IntSet
  clickButton current button = 
    foldl' (toggleLight) current button

  toggleLight light button =
    if button `Set.member` light
    then Set.delete button light
    else Set.insert button light
\end{code}

With BFS handling the search for each individual light, the final solution just sums
up the minimum button presses across all lights in our input.

\begin{code}

solvePart1 :: [Light] -> Int
solvePart1 = sum . map findMinButtonPresses

part1 :: IO ()
part1 = readFile' "assets/day10-input.txt" >>= print . solvePart1 . parse

\end{code}

Part 2
------

Unfortunately I haven't been able to solve part 2 yet... So instead here's a picture of my dog:


![My dog Pepita](pepita.webp)

I have a few idea of how to solve it:

1. Using a [linear programming](https://en.wikipedia.org/wiki/Linear_programming) solver - to find the minimum of the set of linear equations
2. Using an [SMT](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories) solver
3. Brute forcing with a lot of optimizations + paralellism

My first instinct was to go with idea #1 but I've spent too long now trying to make it work in haskell.
Now I don't have enough time to try idea #3 today but I'll come back to it at some point.



\begin{code}
solvePart2 :: [Light] -> Int
solvePart2 = criesInUnsolved
  where criesInUnsolved = error "fml"
\end{code}
