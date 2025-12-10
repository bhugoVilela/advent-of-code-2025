# Advent of Code 2025 - Haskell Solutions

My [Advent of Code 2025](https://adventofcode.com/2025) solutions, written in **literate Haskell** and published as a beautiful, readable web book.

**📖 [Read the full book with explanations](https://bhugovilela.github.io/advent-of-code-2025)**

## What Makes This Special?

This isn't just code—it's a **literate programming book** where prose and code live side-by-side. Each day's solution includes:

- Clear problem explanations with concrete examples
- Step-by-step algorithm walkthroughs
- Insights into why certain approaches were chosen
- Clean, functional Haskell code

The solutions prioritize **readability first**, with performance as a strong second. The goal is code you can understand and learn from, not just code that runs fast.

## Project Structure

- **[📖 Web Book](https://bhugovilela.github.io/advent-of-code-2025)** - Full explanations and solutions
- **[src/](src)** - Literate Haskell source files (`.lhs`)
- **[app/Main.hs](app/Main.hs)** - Executable to run and benchmark solutions
- **[book/](book)** - Pandoc tooling for generating the web book

## Performance

While readability is the priority, performance is still important. All solutions run efficiently on modern hardware.

Benchmarks measured using Criterion on an Apple M1 Pro:

![Benchmarks](https://github.com/bhugoVilela/advent-of-code-2025/blob/main/bench.svg?raw=true)

## Running Solutions

> [!WARNING]
> This repository doesn't include puzzle inputs, as Advent of Code [does not allow redistribution](https://adventofcode.com/2025/about#faq_copying). You'll need to download your inputs and place them in `./assets/dayXX-input.txt`.

```bash
# Run a specific day and part
stack run aoc -- --day 1 --part 1

# Run benchmarks
scripts/runBenchmarks dry
```

## Building the Web Book

The literate Haskell files are transformed into a beautiful two-column web book where explanations appear on the left and code appears on the right.

**Tech Stack:**
- **Standard Literate Haskell** - Uses the markdown subset compatible with GHC and the existing Haskell toolchain (no custom preprocessors needed)
- **Pandoc** - Converts `.lhs` files to HTML
- **Custom Lua filter** - Creates the two-column layout and adds utilities like `[h3] Title` for custom headers within prose
- **Single HTML output** - All days compiled into one cohesive document
- **Custom templates** - Tweaked HTML and CSS for responsive design with dark/light mode support

**Build Commands:**
```bash
# Build the book once (generates docs/index.html)
book/build.sh

# Watch mode (rebuilds on file changes - requires entr)
book/watch.sh
```

The generated site is published to GitHub Pages from the `docs/` directory.

## Credits

The two-column layout and Pandoc + Lua filter approach is inspired by [@oppi.li](https://tangled.org/oppi.li)'s excellent [Book of Solves](https://aoc.oppi.li/). This implementation differs by:

- Using standard literate Haskell (GHC-compatible) rather than custom markdown
- Enhanced Lua filter with additional parsing utilities
- Single-file HTML output instead of multiple pages
- Custom HTML/CSS templates tailored for this project

## AI Disclaimer
An LLM was used to help improve the clarity of the explanations, but none was used to produce code at any point.
