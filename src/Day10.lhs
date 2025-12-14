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
import Data.List.Split (splitOn)
import Control.Arrow ((>>>))
import System.IO (readFile')
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.IntSet as Set
import Data.IntSet (IntSet)
import qualified Z3.Monad as Z
import Data.Maybe (catMaybes)
import Control.Monad (forM, forM_)
import GHC.IO.Unsafe (unsafePerformIO)
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

part1 :: String -> Int
part1 = solvePart1 . parse

\end{code}

Part 2
------

Unfortunately I haven't been able to solve part 2 yet... So instead here's a picture of my dog:

![My dog Pepita](pepita.webp)

[h3] SOLVED!

Hey there, this is Hugo from the future, I have finally been able to link Z3 with haskell on my macbook.
I'm new to Z3 and SMT solvers in general so I had to take some time to learn how they work.

After solving the problem in python I was ready to try again with haskell.
So here we go

[h3] Reframing the problem as a linear equations problem

We're trying to minimize the following equation:

```md
button_0 + button_1 + ... button_n
```

where `button_i` is the number of keypresses of the i_th button in the light.

Additionally for every joltage we find every button that increases it. Then we know that
the sum of those button clicks must be equal to that joltage.

```md
button_A + ... + button_Z = joltage
where
button_X is a button that increases that joltage
```

[h3] Solving the problem

First we write a small function to get the linear equations.

For each joltage we find every button that increases it and store its index.

\begin{code}
getLinearEquations :: Light -> [(Int, [Int])]
getLinearEquations (Light _ buttons joltages) =
  flip map (enumerated joltages) $ \(jix, joltage) ->
    let buttons' = (enumerated buttons) 
            & filter (any (== jix) . snd) 
            & map fst
     in (joltage, buttons')

enumerated :: [b] -> [(Int, b)]
enumerated = zip [0..]
\end{code}

Then, we need to model our problem in Z3, unfortunately the haskell bindings leave a lot to be desired
in terms of readability.

\begin{code}
script :: Light -> Z.Z3 Int
script light@(Light _ buttons _) = do
\end{code}

We create a variable for each button that represent the number of button presses

\begin{code}
  vars <- forM [0..length buttons - 1] $ Z.mkFreshIntVar . show
\end{code}

Then we add a contraint - a button press must be greater or equal to 0

\begin{code}
  forM_ vars $ \var -> 
    Z.optimizeAssert =<< Z.mkGe var =<< Z.mkIntNum 0
\end{code}

Then we go through each equation and write a constraint:

- the sum of the button presses of a joltage must be equal to that joltage

\begin{code}
  -- for each equation
  forM_ (getLinearEquations light) $ \(joltage, buttons) -> do
    let buttonVars = map (vars !!) buttons
    buttonSum <- Z.mkAdd buttonVars
    -- the sum of the buttons must be equal to the joltage
    Z.optimizeAssert =<< Z.mkEq buttonSum =<< Z.mkIntNum joltage
\end{code}

Finally 

- We create our goal (the total number of key presses)
- tell Z3 to minimize it
- tell Z3 to run with `Z.optimizeCheck []`
- read back the current value of each variable
- return the sum

\begin{code}
  goal <- Z.mkAdd vars

  Z.optimizeMinimize goal
  _ <- Z.optimizeCheck []
  m <- Z.optimizeGetModel

  res <- sum . catMaybes <$> mapM (Z.evalInt m) vars 
  return $ fromInteger res
\end{code}

Ooof, what a verbose mess, at last we're ready to wrap it.

We need to resort to `unsafePerformIO` to keep our part2

\begin{code}
solvePart2 :: [Light] -> IO Int
solvePart2 = fmap sum . mapM (Z.evalZ3 . script)

part2 :: String -> Int
part2 = unsafePerformIO . solvePart2 . parse
\end{code}
