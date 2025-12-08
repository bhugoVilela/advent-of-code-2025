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

type CircuitId = Int
type Junction = (V3 Int)

parse :: String -> [Junction]
parse = map parseJunction . lines
  where
  parseJunction :: String -> Junction
  parseJunction line = let [x, y, z] = splitOn "," line
                        in V3 (read x) (read y) (read z)

distanceTo :: Junction -> Junction -> Double
distanceTo (V3 x1 y1 z1) (V3 x2 y2 z2) =
         (fromIntegral (x2 - x1)) ** 2.0
       + (fromIntegral (y2 - y1)) ** 2.0
       + (fromIntegral (z2 - z1)) ** 2.0

distances :: [Junction] -> [(Junction, Junction)]
distances vecs = [ (v1, v2)
                 | (ix, v1) <- zip [0..] vecs
                 , v2 <- drop (ix + 1) vecs
                 ] & sortBy (compare `on` uncurry distanceTo)


data ProblemState = ProblemState {
  _nextCircuitId :: CircuitId,
  _junctions :: HashMap Junction CircuitId,
  _circuits :: HashMap CircuitId [Junction]
}
makeLenses 'ProblemState

emptyState :: ProblemState
emptyState = ProblemState 0 Map.empty Map.empty

type Solver a = State ProblemState a

addJunctionToCircuit :: (V3 Int) -> CircuitId -> Solver ()
addJunctionToCircuit junction circuitId = do
    junctions %= Map.insert junction circuitId
    circuits %= Map.alter (insert junction) circuitId
    return ()
  where
    insert :: V3 Int -> Maybe [V3 Int] -> Maybe [V3 Int]
    insert junction Nothing = Just [junction]
    insert junction (Just js) = Just (junction : js)

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

part1 :: String -> Int
part1 = solvePart1 . parse

part2 :: String -> Int
part2 = solvePart2 . parse

solvePart1 :: [V3 Int] -> Int
solvePart1 junctions = flip evalState emptyState $ do
  let pairs = take 1000 $ distances junctions
  forM_ pairs addJunctionPair
  uses circuits (Map.toList >>> map (length . snd) >>> sortBy (flip compare) >>> take 3 >>> product)

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
