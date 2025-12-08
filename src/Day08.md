# Day 8
## Part 1

Today we get a list of electrical junctions in 3D space

```md
162,817,812
57,618,57
906,360,560
...
```
We're asked to pick the two closest ones and add them to a circuit, which may result in
either: 

- A new circuit with just the two (if none are in a circuit)
- Adding one of them to the other's circuit (if the other is already in a circuit)
- Merging two circuits together.


We need to do this 1000 times and then multiply the length of the 3 most populated circuits.

If the operations above remind you of a DisjointSet then you're on the right track.
Indeed I suspect using a DistjoinSet would result in a more efficient solution. 

However, today's challenge is the reason I learned about disjoint sets and only after I had already a solution.

So today we'll look at my naive solution.

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

module Day08 where
import Linear.V3
import Data.Function (on)
import Data.List.Split (splitOn)
import System.IO (readFile')
import Data.List (sortBy)
import Data.HashMap.Strict (HashMap, (!?) )
import qualified Data.HashMap.Strict as Map
import Control.Lens
import Control.Arrow ((>>>))
import Control.Monad.State.Lazy (State, evalState)
import Control.Monad (forM_)
```

As usual we start with a model. We'll use Ints to identify circuits and 3-dimensional
vectors for the junctions

```haskell
type CircuitId = Int
type Junction = (V3 Int)
```

We split the input into lines and parse a junction for each line

```haskell
parse :: String -> [Junction]
parse = map parseJunction . lines
  where
  parseJunction :: String -> Junction
  parseJunction line = let [x, y, z] = splitOn "," line
                        in V3 (read x) (read y) (read z)
```

A simple euclidean distance function. We can do without `sqrt` because
the relative distances remain the same.

```haskell
distanceTo :: Junction -> Junction -> Double
distanceTo (V3 x1 y1 z1) (V3 x2 y2 z2) =
         (fromIntegral (x2 - x1)) ** 2.0
       + (fromIntegral (y2 - y1)) ** 2.0
       + (fromIntegral (z2 - z1)) ** 2.0
```

And finally, `distances` gets us the sorted list of the nearest junctions.
we just find all pairs of junctions and sort them with `distanceTo`

```haskell
distances :: [Junction] -> [(Junction, Junction)]
distances vecs = [ (v1, v2)
                 | (ix, v1) <- zip [0..] vecs
                 , v2 <- drop (ix + 1) vecs
                 ] & sortBy (compare `on` uncurry distanceTo)
```

Let's expand our model:

`ProblemState` keeps track of a few things,

- **`nextCircuitId`** - we'll need this to know what id to use when we create a new circuit
- **`junctions`** - maps a junction to a circuit (allows to ask the circuit of a junction efficiently)
- **`circuits`** - maps a circuit to a list of junction (allows us to get all junctions in a circuit efficiently)

this should be enough to perform the algorithm.

```haskell
data ProblemState = ProblemState {
  _nextCircuitId :: CircuitId,
  _junctions :: HashMap Junction CircuitId,
  _circuits :: HashMap CircuitId [Junction]
}

emptyState :: ProblemState
emptyState = ProblemState 0 Map.empty Map.empty
```

Today we're using the State monad + the lens library to help keeping our solution readable.

If you're not proficient with lenses I highly recommend [This Book](https://leanpub.com/optics-by-example/).
```haskell
makeLenses 'ProblemState
```

We'll be using the State Monad under the name Solver to model our computations.

```haskell
type Solver a = State ProblemState a
```

Let's start with a few utility functions

To add a junction to a circuit we need to update both the `junctions` and `circuits` data structures

```haskell
addJunctionToCircuit :: (V3 Int) -> CircuitId -> Solver ()
addJunctionToCircuit junction circuitId = do
    junctions %= Map.insert junction circuitId
    circuits %= Map.alter (insert junction) circuitId
    return ()
  where
    insert :: V3 Int -> Maybe [V3 Int] -> Maybe [V3 Int]
    insert junction Nothing = Just [junction]
    insert junction (Just js) = Just (junction : js)
```

Now we can do one full step of the algorithm, picking two junctions and deciding how to
for junctions `(a, b)` there are 5 possible cases that we need to handle

1. junction `a` is in circuit `ca` - we insert junction `b` in circuit `ca`
2. junction `b` is in circuit `cb` - same as above with b
3. both junctions are in the same circuit - nothing changes
4. both junctions are in different circuits - we need to merge the circuits
  - 1. move all junctions in `ca` to `cb`
  - 2. delete the `ca` circuit
5. neither `a` nor `b` are in a circuit - create a new circuit with both
  - add both to a circuit with a new id
  - increase the `nextCircuitId` tracker

Ooof, well, that's the juice of today's problems and the biggest function today.

```haskell
addJunctionPair :: (V3 Int, V3 Int) -> Solver ()
addJunctionPair (a, b) = do
  -- find the circuits where a and b belong
  mCa <- uses junctions (!? a)
  mCb <- uses junctions (!? b)
  case (mCa, mCb) of
    (Just ca, Nothing) -> addJunctionToCircuit b ca
    (Nothing, Just cb) -> addJunctionToCircuit a cb
    (Just ca, Just cb) | ca == cb -> return ()
    (Just ca, Just cb) -> do
      junctionsInCa <- uses circuits (Map.! ca)
      forM_ junctionsInCa $ flip addJunctionToCircuit cb
      circuits %= Map.delete ca
    (Nothing, Nothing) -> do
      nextId <- use nextCircuitId 
      nextCircuitId += 1
      addJunctionToCircuit a nextId
      addJunctionToCircuit b nextId
```

With all of that out of the way we're ready to implement part1:
1. We take the 1000 smallest pairs of distances
2. Go through each of them with `forM_` to add them
3. calculate the score
  - this long-ass one-liner takes the circuits and finds the 3 longest ones and multiplies their lengths

```haskell
part1 :: String -> Int
part1 = solvePart1 . parse

solvePart1 :: [V3 Int] -> Int
solvePart1 junctions = flip evalState emptyState $ do
  let pairs = take 1000 $ distances junctions
  forM_ pairs addJunctionPair
  uses circuits (Map.toList >>> map (length . snd) >>> sortBy (flip compare) >>> take 3 >>> product)
```

## Part 2
For part 2 we're asked to find the first pair of junctions that when added
will form 1 final circuit with all junctions, and multiply their x coordinates.

Turns out we already have everything to solve part2 as well.

`addUntilDone` - adds pairs until the condition is met and returns the correct pair 

We multiply their x coordinates and VOILA, day 8 is done.

```haskell
part2 :: String -> Int
part2 = solvePart2 . parse

solvePart2 :: [V3 Int] -> Int
solvePart2 js = flip evalState emptyState $ do
  let pairs = distances js
      addUntilDone [] = error "unreachable"
      addUntilDone (n:rest) = do
        addJunctionPair n
        junctionCount <- uses junctions Map.size
        circuitCount <- uses circuits Map.size
        if junctionCount == length js && circuitCount == 1
          then return n
          else addUntilDone rest

  (a, b) <- addUntilDone pairs
  return $ a^._x * b^._x
```
