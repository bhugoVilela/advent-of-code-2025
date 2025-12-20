module Main (main) where

import System.IO (readFile')
import Options.Applicative
import Control.Monad (forM_)
import Text.Printf (printf)
import GHC.IO (evaluate)

import AllDays (allDays)
import Data.List (singleton)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (force)

data Args = Run { _day :: Maybe Int , _part :: Maybe Int } deriving (Show)

argsParser :: Parser Args
argsParser =
  Run
  <$> optional (option auto ( long "day" <> short 'd' <> help "Which day to run" ))
  <*> optional (option auto ( long "part" <> short 'p' <> help "Which Part to run" ))


opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc <> progDesc "Advent of Code 2025 in Haskell")

main :: IO ()
main = do
  Run mDay mPart <- execParser opts
  let days  = maybe [1..length allDays] singleton mDay
  let parts = maybe [1,2] singleton mPart
  let progs = [(fn, d, p) | d <- days, p <- parts, let fn = allDays !! (d-1) !! (p-1)]

  forM_ progs $ \(prog, day, part) -> do
    contents <- readFile' $ getInputFile day
    t1 <- getCPUTime
    result <- evaluate $ force $ prog contents
    t2 <- getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-9
    printf "input: %s\n" $ getInputFile day
    printf "Day%02d - Part%d: %d\n" day part result
    printf "Time: %.2fms\n" t
    printf "---------------\n"

  where
    getInputFile :: Int -> String
    getInputFile = printf "assets/day%02d-input.txt"
