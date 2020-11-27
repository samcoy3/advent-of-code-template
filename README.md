# aoc-template

This is an (opinionated) Advent of Code template for solutions in Haskell.

To use:
- Clone this repository
- Set up a new branch for the year's solutions
- Change the package name, update the GitHub link, etc. You'll also want to remove the .cabal file and let stack generate a new one.
- Fill in the solutions and have fun!

When running from the command line you can pass the option `-d/--day DAY` to run a specific day's solutions. If you do this, then you can also pass `-i/--input FILE` to specify an input file; by default, the program will look for it in `input/DayXX.txt`. You can also pass the argument `--all-days` and all days will be run in order, assuming the input files are in their default places.

Example usage:
- `stack run -- -d 9`: Runs Day 9's solutions.
- `stack run -- --day 14 --input "wibble.txt"`: Runs Day 14's solutions, using the input file "wibble.txt".
- `stack run -- --all-days`: Runs the solutions from all days.

If you think the structure of the `Day` files needs changing to better suit your needs (before starting the project), then make the appropriate changes in `src/Days/Day01.hs` and run the `apply_changes.zsh` file. This will copy Day01 to all the other days, changing Day01 for DayXX as appropriate.

## Default Language Extensions

I've turned several language extensions on by default. These are:
- `DeriveFoldable`
- `DeriveFunctor`
- `EmptyCase`
- `FlexibleContexts`
- `FlexibleInstances`
- `GADTs`
- `InstanceSigs`
- `LambdaCase`
- `MultiParamTypeClasses`
- `MultiWayIf`
- `NumericUnderscores`
- `OverloadedStrings`
- `RecordWildCards`
- `TupleSections`
- `ScopedTypeVariables`

The reason for these should be pretty clear in most cases.

## Default Dependencies

The default package dependencies for this project are:
- `directory`: This is just for checking if the provided input file exists.
- `containers`: For Map, Set, and so on.
- `text`: Because `String`s are bad.
- `attoparsec`: For the input parser for each day.
- `optparse-applicative`: For command line parsing.
- `mtl`: Mainly in anticipation that `State` might be useful. `ExceptT` is also used to catch exceptions in `runDay`.
- `vector`: In anticipation that fixed-length arrays will come in handy.
- `pointedlist`: Because Advent of Code loves circular lists
