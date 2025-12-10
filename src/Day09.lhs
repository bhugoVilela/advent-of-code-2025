Day 9
=====

Part 1
-----

Today's puzzle gives us a collection of points scattered across a 2D grid.
Our task: find the largest rectangle we can form by choosing any two points as opposite corners.

The twist? The rectangle's sides must be axis-aligned (parallel to the x and y axes),
and we count tiles inclusively—so a rectangle from (0,0) to (2,2) contains 9 tiles, not 4.

Oooof, these imports are getting out of hand! But hey, we've got SVG rendering to do later.

\begin{code}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Day09 where
import Linear.V2
import Data.List.Split (splitOn)
import Control.Arrow ((>>>))
import System.IO (readFile')
import Data.Function ((&))
import Control.Lens.Getter
import Text.Blaze.Svg11 ((!), m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Printf (printf)
import Data.String (IsString(fromString))
import Text.Blaze.Svg (mkPath)
import Control.Monad (forM_)
import Text.Blaze.Svg11 (l)
\end{code}

The `linear` library's `V2 Int` type is perfect for 2D points—it gives us
vector operations for free if we need them later.

Parsing is straightforward: each line contains comma-separated x,y coordinates.

\begin{code}
type Point = V2 Int

parse :: String -> [Point]
parse = lines >>> map parseV2
  where
  parseV2 line = let [x, y] = splitOn "," line
             in V2 (read x) (read y)
\end{code}

With a modest number of points, brute force is our friend. We check every possible
pair of points, calculate the resulting rectangle's area, and take the maximum.

\begin{code}
solvePart1 :: [V2 Int] -> Int
solvePart1 tiles = 
  maximum [ area p1 p2 
          | (ix, p1) <- zip [1..] tiles
          , p2 <- drop ix tiles 
          ]
\end{code}

The `area` function adds 1 to each dimension because we're counting tiles inclusively—a
rectangle from (0,0) to (2,2) spans 3 units in each direction, giving us 3×3=9 tiles.

\begin{code}
area :: V2 Int -> V2 Int -> Int
area (V2 x1 y1) (V2 x2 y2) = (abs (y2 - y1) + 1) * (abs (x2 - x1) + 1)

part1 :: String -> Int
part1 = solvePart1 . parse
\end{code}

Part 2
------

Part 2 cranks up the difficulty: now we need to find the largest rectangle that fits
*inside* a polygon formed by connecting all the input points in sequence.

[h3] A stroke of luck

My first instinct was to implement the classic [raycast](https://en.wikipedia.org/wiki/Point_in_polygon)
algorithm for point-in-polygon testing. But that's a lot of code for something I've
written before, and—let's be honest—it's not that interesting the second time around.

Instead, I got curious. What does the actual input look like? I whipped up a quick
SVG visualization:

![day09 input svg](day09-input.svg)

**Jackpot!** The polygon is roughly circular with a narrow cut extending inward from the edge.

This observation unlocks a much simpler approach: any rectangle large enough to be worth
considering will be entirely on one side of that cut. It either fits in the "upper"
region or the "lower" region—it can't bridge across the cut without intersecting an edge.

So instead of checking if every point inside a candidate rectangle is within the polygon,
we just need to verify that the rectangle's boundary doesn't intersect any polygon edges.

\begin{code}
drawInput :: IO String
drawInput = do
  points <- parse <$> readFile' "assets/day09-input.txt"
  let edges = let (p:ps) = points in zip (p:ps) (ps ++ [p])
      maxX = points & map (view _x) & maximum
      maxY = points & map (view _y) & maximum 
      svgPath = mkPath $ do
        forM_ edges $ \(e1, e2) -> do 
          m (e1^._x) (e1^._y)
          l (e2^._x) (e2^._y)
      svg = renderSvg $ S.docTypeSvg ! A.version "1.1" ! A.width "400" ! A.height "400" ! A.viewbox (fromString $ printf "0 0 %d %d" maxX maxY) $ do
        S.path ! A.d svgPath ! A.strokeWidth "400" ! A.stroke "red";
  return svg

\end{code}

[h3] The solution

Armed with our insight, the solution mirrors Part 1's brute force—except now we
filter out any rectangle whose interior intersects with a polygon edge.

\begin{code}
solvePart2 :: [V2 Int] -> Int
solvePart2 squares@(p:ps) = 
   let rectangles = [ (p1, p2)
                    | (ix, p1) <- zip [1..] squares
                    , p2 <- drop ix squares
                    , not (intersects p1 p2 edges)
                    ]
       edges = zip (p:ps) (ps ++ [p])
   in maximum $ map (uncurry area) $ rectangles
\end{code}

The `intersects` function checks if any polygon edge crosses through our candidate
rectangle. We do this by testing whether the edge's bounding box overlaps with the
rectangle's interior. If any edge overlaps, the rectangle is invalid.

\begin{code}
intersects :: V2 Int -> V2 Int -> [(V2 Int, V2 Int)] -> Bool
intersects p1 p2 edges = any inside $ edges
  where
    inside :: (V2 Int, V2 Int) -> Bool
    inside (e1, e2) = min (e1^._x) (e2^._x) < maxX 
                     && max (e1^._x) (e2^._x) > minX 
                     && min (e1^._y) (e2^._y) < maxY 
                     && max (e1^._y) (e2^._y) > minY 

    minX = min (p1^._x) (p2^._x)
    maxX = max (p1^._x) (p2^._x)
    minY = min (p1^._y) (p2^._y)
    maxY = max (p1^._y) (p2^._y)

part2 :: String -> Int
part2 = solvePart2 . parse
\end{code}
