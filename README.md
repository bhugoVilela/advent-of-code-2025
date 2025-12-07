# Advent Of Code 2025 - Haskell

[Advent of Code 2025](https://adventofcode.com/2025) has started and this year I'm solving it in Haskell.

- You can find the solutions [here](src); 
- [main](app/Main.hs) is an executable to run/benchmark the solutions.

## Considerations
- #1 goal is readable solutions
- #2 is decent performance, although not at the cost of #1

## Performance
Benchmarks are measured using criterion on an Apple M1 Pro

![Benchmarks](https://github.com/bhugoVilela/advent-of-code-2025/blob/main/benchmarks.png?raw=true)

| Day                    | Part   | Time (parsing + solving) |
| -----------------------| ------ | ------------------------ |
| [Day 01](src/Day01.hs) | Part 1 | 3.519 ms                 |
| [Day 01](src/Day01.hs) | Part 2 | 3.544 ms                 |
| [Day 02](src/Day02.hs) | Part 1 | 1.941 ms                 |
| [Day 02](src/Day02.hs) | Part 2 | 1.942 ms                 |
| [Day 03](src/Day03.hs) | Part 1 | 36.90 ms                 |
| [Day 03](src/Day03.hs) | Part 2 | 24.73 ms                 |
| [Day 04](src/Day04.hs) | Part 1 | 8.412 ms                 |
| [Day 04](src/Day04.hs) | Part 2 | 96.62 ms                 |
| [Day 05](src/Day05.hs) | Part 1 | 3.211 ms                 |
| [Day 05](src/Day05.hs) | Part 2 | 935.8 μs                 |
| [Day 06](src/Day06.hs) | Part 1 | 4.315 ms                 |
| [Day 06](src/Day06.hs) | Part 2 | 3.733 ms                 |
| [Day 07](src/Day07.hs) | Part 1 | 23.77 ms                 |
| [Day 07](src/Day07.hs) | Part 2 | 22.45 ms                 |

## Running

> [!WARNING]
> The repo doesn't contain inputs because Advent of Code [does not allow it](https://adventofcode.com/2025/about#faq_copying)
> you will need to add input txt files to `./assets/dayXX-input.txt`

```bash
# Run a single day/part
stack run -- --day 1 --part 1

# Execute the benchmarks and output to html
stack run -- --output benchmarks.html
```
