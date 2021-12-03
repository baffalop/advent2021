# Advent of Code 2021

Solutions to [Advent of Code 2021](https://adventofcode.com/2021) in Haskell.

`app/Main.hs` defines a CLI for running solutions. (Run with `--help` to see options.) It uses the [AoC REST API](https://hackage.haskell.org/package/advent-of-code-api-0.2.8.0) to fetch inputs—you must provide a session key via `-k`. Inputs are cached in the `input` folder (in the working directory, which should be the project root).

Individual solutions are in `src/DayX/Solution.hs`.
