{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Main (main) where

import Lib
import System.IO (readFile')
import Debug.Trace (traceShowId, traceShow)

import Day01
import Day02
import Day03
import Day04
import Options.Applicative
import GHC.List ((!?))
import Data.Maybe (fromMaybe)
import Control.Monad (when, forM)
import Criterion.Main
import Criterion (benchmark)
import qualified Criterion.Main.Options as C
import Text.Printf (printf)
import Data.Foldable (traverse_)
import GHC.IO (evaluate)

data Args = Run { _day :: Int , _part :: Int }
          | Benchmark { _csvFile :: Maybe String, _output :: Maybe String }
          deriving (Show)


argsParser :: Parser Args
argsParser =
      (Run <$> option auto ( long "day" <> short 'd' <> help "Which day to run" ) <*> option auto ( long "part" <> short 'p' <> help "Which Part to run" ))
     <|> (Benchmark <$> optional (strOption (long "csv" <> help "CSV file to write results"))
                     <*> optional (strOption (long "output" <> help "HTML file to write results")))

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc <> progDesc "Advent of Code 2025 in Haskell")

main :: IO ()
main = do
  let solutions :: [(String -> Int, String -> Int)]
      solutions = [ (Day01.part1, Day01.part2)
                  , (Day02.part1, Day02.part2)
                  , (Day03.part1, Day03.part2)
                  , (Day04.part1, Day04.part2)
                  ]
  args <- execParser opts

  case args of
    Run day part -> do
      let mProg = fromMaybe (error "error: invalid day") 
                  $ (if part == 1 then fst else snd) <$> (solutions !? (day - 1))
      input <- readFile' $ getInputFile day
      print $ mProg input
    Benchmark csvFile output -> do
      runs <- forM (zip [1..] solutions) $ \(day, (part1, part2)) -> do
        input <- readFile' $ getInputFile day
        return [
          (printf "Day %02d Part 1" day :: String, input, part1),
          (printf "Day %02d Part 2" day, input, part2)
          ]
      actualRuns <- evaluate $ concat runs
      defaultMain $ [
        bgroup "2025" $ map (\(name, input, action) -> bench name $ whnf action input) actualRuns
        ]
      
  where
    getInputFile :: Int -> String
    getInputFile = printf "assets/day%02d-input.txt"
