Day 11
======

Part 1
------

Day 11 presents us with a network topology puzzle! We're given a list of network devices
and their connections, forming a directed acyclic graph (DAG). Each line describes which
devices a particular node connects to.

Here's a simplified example of the input format:

```md
a: you
you: c d
c: out
d: out
```

This describes a network where device `a` connects to `you`, device `you` has two
outgoing connections to `c` and `d`, and both `c` and `d` connect to the final destination `out`.

Our task for Part 1: **count all possible paths** from the starting node `"you"` to the
destination node `"out"`. In the example above, there are 2 paths:
- `you → c → out`
- `you → d → out`

\begin{code}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11 where
import Data.Text (Text)
import Data.Text qualified as Text
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
\end{code}

We're using `Text` for efficient string handling (device IDs can be arbitrary strings),
and `HashMap` for fast O(log n) lookups when traversing the graph—much better than
linear searches through lists!

[h3] Modeling and parsing

The model is straightforward: we represent the entire network as a mapping from each
device ID to its list of outgoing connections.

Parsing is mercifully simple: each line has a device ID followed by a colon, then
a space-separated list of devices it connects to. We strip the trailing colon with
`Text.init` and collect the rest as a list of outgoing connections.

\begin{code}
type DeviceId = Text
type Connections = HashMap DeviceId [DeviceId]

parse :: String -> Connections
parse = Map.fromList . map parseConnections . Text.lines . Text.pack
  where
  parseConnections :: Text -> (DeviceId, [DeviceId])
  parseConnections line =
    let (deviceId:others) = Text.words line
     in (Text.init deviceId, others)
\end{code}

[h3] Counting paths with dynamic programming

Here's where things get interesting. The naive approach—recursively exploring every
path from start to end—would repeatedly recompute the same subproblems. If device `c`
is reachable from multiple places in the network, we'd count all paths from `c` to `out`
multiple times!

**Dynamic programming to the rescue!** We'll use memoization to cache the number of
paths from each device to our destination. The first time we visit a device, we compute
its path count and store it. Every subsequent visit just looks up the cached value.

Think of it as working backwards from the destination:
- The destination itself has exactly 1 path (the empty path)
- Any device that connects directly to the destination has as many paths as it has
  outgoing edges to the destination
- Any other device has paths equal to the sum of paths from all its children

This is essentially a **depth-first search with memoization**, a classic DP technique
for DAGs.

\begin{code}

\end{code}

The `countConnections` function implements our memoized DFS. Let's break down what's happening:

**Initial cache setup**: We start with a clever optimization—any device that connects
directly to `end` already has at least one path. We pre-populate the cache by counting
direct connections to the end node for every device.

**The `buildMap` recursion** handles three cases:

1. **Base case**: If we've reached the end device, return a cache with just `{end: 1}`
2. **Cache hit**: If we've already computed this device's path count, return the
   existing cache unchanged
3. **Cache miss**: This is where the magic happens:
   - Get all children (devices this one connects to)
   - Recursively build the cache for each child using `foldl'`
   - Sum up the path counts from all children (that's the total paths through this device!)
   - Insert this device's count into the cache

The fold accumulates the cache as we explore, ensuring each device is only computed once.
When we're done, we look up our starting device in the final cache to get the total
path count.

\begin{code}
countConnections :: Connections -> DeviceId -> DeviceId -> Int
countConnections grid start end =
  let cache = Map.filter (> 0) . fmap (length . filter (== end)) $ grid
      newCache = buildMap start cache
   in newCache Map.! start
  where
    buildMap :: DeviceId -> HashMap DeviceId Int -> HashMap DeviceId Int
    buildMap device cache
      | device == end = Map.singleton end 1
      | Map.member device cache = cache
      | otherwise =
        let children = fromMaybe [] $ grid Map.!? device
            combine = flip buildMap
            childrensMap = foldl' combine cache children
            childrenCount = sum . mapMaybe (`Map.lookup` childrensMap) $ children
         in Map.insert device childrenCount childrensMap

solvePart1 :: Connections -> Int
solvePart1 graph = countConnections graph "you" "out"

part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

Part 2
------

Part 2 cranks up the complexity: now we need to count only paths from `"svr"` to `"out"`
that pass through **both** `"fft"` and `"dac"` devices. Not just one or the other—both!

[h3] The inclusion-exclusion principle

At first glance, this seems like we need a completely different algorithm. But there's
an elegant mathematical trick we can use: the [inclusion-exclusion principle](https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle).

Think about it in terms of set operations. We want paths that include both checkpoints,
which is equivalent to:

```
allPaths - pathsWithoutDac - pathsWithoutFft + pathsWithoutEither
```

Why does this work?
- Start with **all paths** from svr to out
- Subtract paths that **don't go through dac** (removes all paths missing that checkpoint)
- Subtract paths that **don't go through fft** (removes all paths missing that checkpoint)
- Add back paths that **avoid both** (we subtracted these twice, so we need to add them once)

This is the classic inclusion-exclusion formula: |A ∩ B| = |U| - |Ā| - |B̄| + |Ā ∩ B̄|

And the beautiful part? We can compute "paths that avoid X" by simply removing X from
the graph and counting paths normally!

\begin{code}
solvePart2 :: Connections -> Int
solvePart2 graph = all - noDac - noFft + noDacNoFft
  where
    all   = countConnections graph "svr" "out"
    noDac = countConnections (Map.delete "dac" graph) "svr" "out"
    noFft = countConnections (Map.delete "fft" graph) "svr" "out"
    noDacNoFft = countConnections (Map.delete "dac" $ Map.delete "fft" graph) "svr" "out"
\end{code}

[h3] Alternative approach: counting subpaths

There's another way to think about this problem that's perhaps more intuitive. Any path
that goes through both dac and fft must visit them in some order:

- Either: `svr → ... → dac → ... → fft → ... → out`
- Or: `svr → ... → fft → ... → dac → ... → out`

These are mutually exclusive cases (you can't visit dac before fft *and* fft before dac
in the same path), so we can count them separately and add the results.

For the first case, we need:
```
(paths from svr to dac) × (paths from dac to fft) × (paths from fft to out)
```

Why multiplication? Because each path in the first segment can be paired with each path
in the second segment, and each of those combinations can be paired with each path in
the third segment. That's the fundamental counting principle!

Similarly for the second case. Add them together:

\begin{code}
solvePart2' :: Connections -> Int
solvePart2' graph = paths "svr" "dac" * paths "dac" "fft" * paths "fft" "out"
                  + paths "svr" "fft" * paths "fft" "dac" * paths "dac" "out"
  where
    paths = countConnections graph

part2 :: String -> Int
part2 = solvePart2 . parse
\end{code}

This alternative formulation is arguably clearer—it directly expresses the structure
of the paths we're counting. Both solutions produce the same answer, but I find the
inclusion-exclusion approach (`solvePart2`) more elegant and generalizable. If we needed
to check for three or four required checkpoints, the combinatorial explosion of cases
in the subpath approach would get messy fast, while inclusion-exclusion scales gracefully.
