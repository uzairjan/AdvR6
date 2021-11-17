
# lab06

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/uzairjan/AdvR6.svg?branch=main)](https://api.travis-ci.com/v3/job/547018288/log.txt)
<!-- badges: end -->

The goal of lab06 is to ...

## Installation

You can install the released version of lab06 from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lab06")
devtools::install_github("uzairjan/AdvR6")

```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lab06)
## basic example code

n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
# dynamic knapsack
obj <- knapsack_dynamic()
obj$knapsackDynamic(df = knapsack_objects[1:8,], W = 3500)

#brute force knapsack 

btfObc <- brute_force_knapsack()

btfObc$bruteForceKnapsack(x = knapsack_objects[1:8,], w = 3500)

#greedy knapsack 
obj <- greedy_knapsack()
obj$greedyKnapSack(knapsack_objects[1:8,], 3000)
```

