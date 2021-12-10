# Advent of Code 2021

## Haskell solutions to most problems

### You can visit the official site [**here**](https://adventofcode.com/2021) to look at the problems

All days are solved in the style of the template directory:
1. A file with "test" data - the one given in examples. Useful when implementing the code.
1. A file with the proper input.
1. A day\<number-of-day\>.hs file containing the code. You can switch between using the test data and the proper input by changing the filename in the `main` function.

Note that in certain places some code is commented in the `mainWork` function. This is usually code that is used in only the first problem of the day.

Certain days are only partially done, or only have parsed input, as solving them requires use of mutable values (which are annoying and hard to use in Haskell). The exception being day5, where the solution works for the test input (due to its small size), but would require mutable data, too, to solve the true input. 
