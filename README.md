# Advent Of Code 2025 - Haskell

[Advent of Code 2025](https://adventofcode.com/2025) has started and this year I'm solving it in Haskell.

- You can find the solutions [here](src); 
- [main](app/Main.hs) is an executable to run/benchmark the solutions.

## Considerations
- #1 goal is readable solutions
- #2 is decent performance, although not at the cost of #1

## Performance
Benchmarks are measured using criterion on an Apple M1 Pro
| Day                    | Part   | Time (parsing + solving) | std dev  |
| -----------------------| ------ | ------------------------ | -------- |
| [Day 01](src/Day01.hs) | Part 1 | 3.519 ms                 | 91.85 μs |
| [Day 01](src/Day01.hs) | Part 2 | 3.544 ms                 | 128.5 μs |
| [Day 02](src/Day02.hs) | Part 1 | 1.941 ms                 | 78.96 μs |
| [Day 02](src/Day02.hs) | Part 2 | 1.942 ms                 | 49.10 μs |
| [Day 03](src/Day03.hs) | Part 1 | 36.90 ms                 | 778.4 μs |
| [Day 03](src/Day03.hs) | Part 2 | 24.73 ms                 | 839.2 μs |
| [Day 04](src/Day04.hs) | Part 1 | 8.412 ms                 | 553.0 μs |
| [Day 04](src/Day04.hs) | Part 2 | 96.62 ms                 | 269.1 μs |

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
