module AllDays (allDays) where
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09

allDays :: [[String -> Int]]
allDays = 
  [ [Day01.part1, Day01.part2]
  , [Day02.part1, Day02.part2]
  , [Day03.part1, Day03.part2]
  , [Day04.part1, Day04.part2]
  , [Day05.part1, Day05.part2]
  , [Day06.part1, Day06.part2]
  , [Day07.part1, Day07.part2]
  , [Day08.part1, Day08.part2]
  , [Day09.part1, Day09.part2]
  ]
