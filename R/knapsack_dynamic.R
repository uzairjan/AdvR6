

#' Knapsack dynamic
#' @param df data frame
#' @param W weight
#' @export
#' @exportClass knapsack_dynamic
#'
#' @importFrom utils combn
knapsack_dynamic <- setRefClass(
  "knapsack_dynamic",
  methods = list(
    knapsackDynamic = function(df, W) {
      if (length(colnames(df)) != 2 || !is.numeric(W) || !all(colnames(df) == c("w", "v")) || !is.data.frame(df) || !all(df[, "w"] > 0) || !all(df[, "w"] > 0) || W < 0) stop("Wrong input")
      n <- length(df[, "w"])
      mz <- matrix(0, n + 1, W + 1)
      for (i in 2:(n + 1)) {
        for (c in 2:(W + 1)) {
          if (df[, "w"][i - 1] > c) {
            mz[i, c] <- mz[i - 1, c]
          } else {
            mz[i, c] <- max(df[, "v"][i - 1] + mz[i - 1, c - df[, "w"][i - 1]], mz[i - 1, c])
          }
        }
      }
      factors <- c()
      nW <- W + 1
      for (j in (n + 1):2) {
        if (mz[j, nW] != mz[j - 1, nW]) {
          factors <- c(factors, j - 1)
          nW <- nW - df[, "w"][j - 1]
        }
      }
      result <- list(value = round(mz[n + 1, W + 1], digits = 0), factors = factors[order(factors)])
      return(result)
    }
  )
)
