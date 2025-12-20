Day 8
=====
Part 1
------

Today's puzzle gives us electrical junctions floating in 3D space, each described by
x,y,z coordinates:

```md
162,817,812
57,618,57
906,360,560
...
```

Our task: repeatedly pick the two closest junctions and connect them into a circuit.
This happens 1000 times, and each connection can play out in three ways:

- **Both unconnected**: Create a new circuit containing both junctions
- **One connected**: Add the unconnected junction to the existing circuit
- **Both connected**: Merge their two circuits into one

After 1000 connections, we multiply the sizes of the three largest circuits together.

If this sounds like a job for a **Union-Find (Disjoint Set)** data structure, you're
absolutely right. That's the textbook efficient solution for this kind of problem.

Here's the thing: this puzzle is actually *why* I learned about disjoint sets in the
first place—but only *after* I'd already solved it my own way. So today we'll walk through
my original, more naive approach using the State monad and some good old hashmaps.

\begin{code}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Day08 where
import Linear.V3
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.HashMap.Strict (HashMap, (!?) )
import qualified Data.HashMap.Strict as Map
import Control.Lens
import Control.Arrow ((>>>))
import Control.Monad.State.Lazy (State, evalState)
import Control.Monad (forM_)
\end{code}

Let's start with our data model. We'll use integers to identify circuits and
3D vectors from the `linear` library to represent junction positions.

\begin{code}
type CircuitId = Int
type Junction = (V3 Int)
\end{code}

Parsing is straightforward—each line contains comma-separated x,y,z coordinates.

\begin{code}
parse :: String -> [Junction]
parse = map parseJunction . lines
  where
  parseJunction :: String -> Junction
  parseJunction line = let [x, y, z] = splitOn "," line
                        in V3 (read x) (read y) (read z)
\end{code}

For distance calculations, we use squared Euclidean distance. Since we only care about
relative distances for sorting, we can skip the `sqrt`—it's monotonic, so the ordering
stays the same and we save some computation.

\begin{code}
distanceTo :: Junction -> Junction -> Double
distanceTo (V3 x1 y1 z1) (V3 x2 y2 z2) =
         fromIntegral (x2 - x1) ** 2.0
       + fromIntegral (y2 - y1) ** 2.0
       + fromIntegral (z2 - z1) ** 2.0
\end{code}

The `distances` function generates all possible junction pairs and sorts them by distance,
giving us a priority queue of connections to process.

\begin{code}
distances :: [Junction] -> [(Junction, Junction)]
distances vecs = [ (v1, v2)
                 | (ix, v1) <- zip [0..] vecs
                 , v2 <- drop (ix + 1) vecs
                 ] & sortBy (compare `on` uncurry distanceTo)
\end{code}

Now for the core state management. Our `ProblemState` tracks three things:

- **`nextCircuitId`**: Counter for assigning IDs to new circuits
- **`junctions`**: Maps each junction to its circuit ID (for fast "which circuit am I in?" lookups)
- **`circuits`**: Maps each circuit ID to its junctions (for fast "what's in this circuit?" lookups)

This bidirectional indexing lets us efficiently handle all three connection cases.

\begin{code}
data ProblemState = ProblemState {
  _nextCircuitId :: CircuitId,
  _junctions :: HashMap Junction CircuitId,
  _circuits :: HashMap CircuitId [Junction]
}

emptyState :: ProblemState
emptyState = ProblemState 0 Map.empty Map.empty
\end{code}

We're using the **State monad** combined with the **lens library** to keep our solution
clean and readable. Lenses let us update nested state elegantly without the usual
record update boilerplate. If you're new to lenses, I highly recommend
[Optics By Example](https://leanpub.com/optics-by-example/).

\begin{code}
makeLenses 'ProblemState
\end{code}

We alias our computation type as `Solver` for clarity.

\begin{code}
type Solver a = State ProblemState a
\end{code}

[h3] Building blocks

Adding a junction to a circuit requires updating both indexes: we record which circuit
the junction belongs to, and add the junction to that circuit's member list.

\begin{code}
addJunctionToCircuit :: V3 Int -> CircuitId -> Solver ()
addJunctionToCircuit junction circuitId = do
    junctions %= Map.insert junction circuitId
    circuits %= Map.alter (insert junction) circuitId
    return ()
  where
    insert :: V3 Int -> Maybe [V3 Int] -> Maybe [V3 Int]
    insert junction Nothing = Just [junction]
    insert junction (Just js) = Just (junction : js)
\end{code}

[h3] The core algorithm

Now for the heart of the solution: processing a junction pair. Given junctions `(a, b)`,
we need to handle five distinct cases:

1. **Only `a` is connected**: Add `b` to `a`'s circuit
2. **Only `b` is connected**: Add `a` to `b`'s circuit
3. **Both in same circuit**: Nothing to do
4. **Both in different circuits**: Merge them by moving all junctions from one circuit to the other, then delete the empty circuit
5. **Neither connected**: Create a new circuit containing both and increment our ID counter

This is where things get interesting—and where a proper Union-Find structure would shine.
But our State monad approach keeps the logic explicit and easy to follow.

\begin{code}
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
\end{code}

[h3] Putting it together

With our machinery in place, Part 1 is straightforward:

1. Take the 1000 closest junction pairs
2. Process each pair with our state-modifying function
3. Find the three largest circuits and multiply their sizes

That final one-liner does some heavy lifting: it extracts all circuits, maps to their
sizes, sorts in descending order, takes the top 3, and multiplies them together.

\begin{code}
part1 :: String -> Int
part1 = solvePart1 . parse

solvePart1 :: [V3 Int] -> Int
solvePart1 junctions = flip evalState emptyState $ do
  let pairs = take 1000 $ distances junctions
  forM_ pairs addJunctionPair
  uses circuits (Map.toList >>> map (length . snd) >>> sortBy (flip compare) >>> take 3 >>> product)
\end{code}

Part 2
------

Part 2 asks a different question: *when* do all junctions get connected into a single
unified circuit? We need to find the specific pair that completes this unification,
then multiply their x-coordinates together.

The beautiful part? We've already built everything we need. Our state tracks both
the number of junctions assigned to circuits and the number of distinct circuits.
When those numbers are equal to the total junction count and 1 respectively, we're done.

The `addUntilDone` helper processes pairs one by one until it hits that condition,
then returns the winning pair. Multiply the x-coordinates, and we have our answer.

\begin{code}
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
\end{code}
