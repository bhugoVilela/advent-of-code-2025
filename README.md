# Advent Of Code 2025 - Haskell

[Advent of Code 2025](https://adventofcode.com/2025) has started and this year I'm solving it in Haskell.

- You can find the solutions [here](src); 
- [main](app/Main.hs) is an executable to run/benchmark the solutions.

## Considerations
- #1 goal is readable solutions
- #2 is decent performance, although not at the cost of #1

## Performance
Benchmarks are measured using criterion on an Apple M1 Pro

![Benchmarks](https://github.com/bhugoVilela/advent-of-code-2025/blob/main/bench.svg?raw=true)

## Running

> [!WARNING]
> The repo doesn't contain inputs because Advent of Code [does not allow it](https://adventofcode.com/2025/about#faq_copying)
> you will need to add input txt files to `./assets/dayXX-input.txt`

```bash
# Run a single day/part
stack run aoc -- --day 1 --part 1

# Executes criterion benchmarks and output to html
stack run aoc-bench -- --output benchmarks.html
```
