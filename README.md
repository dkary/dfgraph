
# dataflow

An R package to generate data flow graphs from R code.

## Pre-Alpha

This is in a pre-alpha state and not yet intended for use.

## Limitations

- We can misidentify dependencies due to name scoping (e.g., column "d" vs. dataframe "d"). Is this solvable? (seems unlikely)