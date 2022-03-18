# dewey
An R library for a variety of things



A small collection of functions that may be helpful for STATA users or regular human beings.

## Install Process

```R
install.packages("devtools")
devtools::install_github("guslipkin/dewey")
```

## `regsearch`

`(data, dependent, independent, minvar = 1, maxvar, family, topN = 0, interactions = FALSE, multi = FALSE, ...)`

An exhaustive search regression built on base R

## `ifelsedata`

`(x, y, arg = NULL, matchCols = FALSE)`

Fast `data.frame` comparisons at the cell level

## `diffFill`

`(x, lag = 1, differences = 1, ...)`

A wrapper for the base `diff` function that returns a `data.frame` of the same length as the input. Allows for vector input for `lag` or `differences`. 

## `lagMultiple`

`(x, k = 1)`

Appropriately lags an input variable and returns a `data.frame` of the same length as the input. Allows for vector input for `k`.
