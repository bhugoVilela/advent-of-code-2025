{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Bench (main) where

import System.IO (readFile')
import Test.Tasty.Bench
import Text.Printf (printf)
import GHC.IO (evaluate)
import AllDays (allDays)
import Control.DeepSeq (force)

main :: IO ()
main = do
  let progs = [ (day, part, fn)
              | (day, parts) <- zip ([1..] :: [Int]) allDays
              , (part, fn) <- zip ([1..] :: [Int]) parts
              ]
  _ <- evaluate $ force progs

  defaultMain $ [
    bgroup "2025" $ map (\(day, part, fn) ->
      env (readFile' (getInputFile day)) $ \input ->
        bench (printf "Day%02d-Part%d" day part) $ nf fn input) progs
    ]
  where
    getInputFile :: Int -> String
    getInputFile = printf "assets/day%02d-input.txt"
