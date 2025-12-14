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

Part 2 adds a delightful twist: now we need to consider the **joltages**—those power requirements
we ignored in Part 1. Each button press consumes power from specific joltage sources, and we
need to find the minimum number of button presses that both:

1. Achieves the target LED configuration
2. Uses *exactly* the specified amount of power from each joltage source

For example, if joltage 0 must total exactly 7, and buttons `(0,2)` and `(1,3)` both draw from
joltage 0, then we need to press those buttons a combined total of 7 times. The challenge is
finding a combination of button presses that satisfies all joltage constraints simultaneously
while minimizing total presses.

This transforms our simple BFS problem into something much more sophisticated: a constrained
optimization problem.

Unfortunately I couldn't solve part 2 initially... So instead here's a picture of my dog:

![My dog Pepita](pepita.webp)

[h3] SOLVED! Enter the SMT solver

*This is Hugo from the future!* I've finally been able to link Z3 with Haskell on my MacBook.
I'm relatively new to Z3 and SMT (Satisfiability Modulo Theories) solvers in general, so this
was a great learning opportunity.

**What's an SMT solver?** Think of it as a constraint satisfaction engine on steroids. You
describe your problem as a set of mathematical constraints (equations, inequalities, logical
formulas), specify what you want to minimize or maximize, and the solver finds a solution
that satisfies everything—if one exists.

Z3, developed by Microsoft Research, is one of the most powerful SMT solvers available. It's
used for everything from program verification to test case generation. And for our puzzle?
It's perfect!

[h3] Reframing as constrained optimization

The key insight is recognizing this as a system of linear equations with an optimization goal.
Let's break it down:

**Variables**: For each button `i`, let `button_i` represent the number of times we press it.

**Objective function** (what we want to minimize):

```md
total_presses = button_0 + button_1 + ... + button_n
```

**Constraints** (conditions that must be satisfied):

1. **Non-negativity**: We can't press a button a negative number of times

   ```md
   button_i ≥ 0  (for all i)
   ```

2. **Joltage requirements**: For each joltage source, the sum of button presses that draw
   from that source must equal the required total

   ```md
   Σ(button_i where button i affects joltage j) = joltage_j
   ```

For example, if buttons 2, 5, and 7 all draw from joltage source 3, and joltage 3 requires
15 total power, then:

```md
button_2 + button_5 + button_7 = 15
```

This is a classic **Integer Linear Programming (ILP)** problem—and Z3 excels at solving these!

[h3] Building the constraint system

First, we need to extract our linear equations from the puzzle input. For each joltage source,
we identify which buttons affect it.

Remember: button index `i` in the buttons list affects joltage `j` if `j` appears in that
button's list. So for joltage source 2, we scan through all buttons looking for those that
mention index 2.

The result is a list of tuples: `(required_joltage, [button_indices])` where each tuple
represents one constraint equation.

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

[h3] Translating to Z3

Now comes the fun part: encoding our problem in Z3. The Haskell bindings are... let's say
"functional but verbose." Everything is done monadically through the `Z3` monad, which
handles the interaction with the underlying solver.

\begin{code}
script :: Light -> Z.Z3 Int
script light@(Light _ buttons _) = do
\end{code}

**Step 1: Create decision variables**

We create one integer variable per button. These represent our unknowns—the number of times
each button will be pressed in the optimal solution.

\begin{code}
  vars <- forM [0..length buttons - 1] $ Z.mkFreshIntVar . show
\end{code}

**Step 2: Add non-negativity constraints**

Each variable must be ≥ 0 since we can't press a button a negative number of times. We use
`Z.optimizeAssert` to add each constraint to our optimization problem.

\begin{code}
  forM_ vars $ \var ->
    Z.optimizeAssert =<< Z.mkGe var =<< Z.mkIntNum 0
\end{code}

**Step 3: Add joltage equality constraints**

For each joltage equation we extracted earlier, we tell Z3: "the sum of these specific button
presses must equal this joltage value." This is the heart of our constraint system.

\begin{code}
  -- for each equation
  forM_ (getLinearEquations light) $ \(joltage, buttons) -> do
    let buttonVars = map (vars !!) buttons
    buttonSum <- Z.mkAdd buttonVars
    -- the sum of the buttons must be equal to the joltage
    Z.optimizeAssert =<< Z.mkEq buttonSum =<< Z.mkIntNum joltage
\end{code}

**Step 4: Define the optimization goal and solve**

We create an expression representing the total button presses (sum of all variables), tell
Z3 to minimize it, then run the solver. If a solution exists, we extract the value assigned
to each variable and sum them up.

\begin{code}
  goal <- Z.mkAdd vars

  Z.optimizeMinimize goal
  _ <- Z.optimizeCheck []
  m <- Z.optimizeGetModel

  res <- sum . catMaybes <$> mapM (Z.evalInt m) vars
  return $ fromInteger res
\end{code}

The verbosity is unfortunate (those nested `mkEq` and `mkIntNum` calls!), but the elegance
of the approach shines through: we've transformed a complex search problem into a declarative
specification that Z3 can solve optimally.

[h3] Wrapping it up

The final step is straightforward: run our Z3 script for each light and sum the results.
We use `Z.evalZ3` to execute the Z3 monad and get back our answer.

\begin{code}
solvePart2 :: [Light] -> IO Int
solvePart2 = fmap sum . mapM (Z.evalZ3 . script)

part2 :: String -> Int
part2 = unsafePerformIO . solvePart2 . parse
\end{code}

**Note on `unsafePerformIO`**: Normally mixing IO into pure functions is dangerous, but here
it's safe because Z3 solving is referentially transparent—given the same input, it always
produces the same output with no side effects we care about. We use `unsafePerformIO` purely
to maintain a consistent API with Part 1.

The beauty of this approach is its generality. Once you've modeled your problem as constraints
and an objective function, Z3 handles all the complexity of finding an optimal solution. No
manual search algorithms, no clever heuristics—just declarative problem specification and
powerful automated reasoning. This is why SMT solvers are such a fundamental tool in modern
software engineering and formal methods!
