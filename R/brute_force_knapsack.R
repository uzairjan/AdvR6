library(parallel)

#' @title Knapsack Algorithm - Brute Force
#' @description Brute Force Approch to Solve Knapsack Problem
#'
#' @param x data.frame with column names 'v' & 'w'
#' @param weight Knapsack Maximum Weight
#' @param p Bool value to pass to active parallel
#' @return Return best Knapsack combination with maximum value
#' @import parallel
#' @importFrom utils combn
#' @export
#' @exportClass brute_force_knapsack
#'
brute_force_knapsack <- setRefClass(
  "brute_force_knapsack",
  methods = list(
    bruteForceKnapsack = function(x, weight, p = FALSE) {
      if (weight < 1) stop("Please Enter a correct Weight")
      if (!is.data.frame(x)) stop("Data frame required")
      if (!(all(colnames(x) %in% c("v", "w")))) stop("Variable name in the dataframe are not named correctly")
      getValues <- function(count) {
        return(combn(x[, "v"], count, sum))
      }

      getfactors <- function(count) {
        return(combn(rownames(x), count, function(x) getElementName(x)))
      }

      getElementName <- function(names) {
        return(paste(names, collapse = ","))
      }

      getWeight <- function(count) {
        return(combn(x[, "w"], count, sum))
      }

      best_combination <- list()
      best_combination[["value"]] <- 0
      best_combination[["factors"]] <- 0

      totalWeight <- c()
      AllValues <- c()
      AllFactors <- c()
      if (p) {
        nfCores <- parallel::detectCores()
        clusters <- parallel::makeCluster(nfCores)

        totalWeight <- unlist(parLapplyLB(clusters, 1:nrow(x), function(y) getWeight(y)))
        AllValues <- unlist(parLapplyLB(clusters, 1:nrow(x), function(y) getValues(y)))
        AllFactors <- unlist(parLapplyLB(clusters, 1:nrow(x), function(y) getfactors(y)))
        parallel::stopCluster(clusters)
      } else {
        l <- nrow(x)

        i <- 1
        while (i <= l) {
          setWeight <- getWeight(i)
          setValues <- getValues(i)
          setFactors <- getfactors(i)
          totalWeight <- c(totalWeight, setWeight)
          AllValues <- c(AllValues, setValues)
          AllFactors <- c(AllFactors, setFactors)
          i <- i + 1
        }
      }

      Range <- which(totalWeight <= weight)

      valid_weight <- totalWeight[Range]
      valid_values <- AllValues[Range]
      valid_factors <- AllFactors[Range]

      max_value_element <- which(valid_values == max(valid_values))
      best_combination[["value"]] <- round(valid_values[max_value_element], digits = 0)
      best_combination[["factors"]] <- as.numeric(unlist(strsplit(valid_factors[max_value_element], ",")))
      return(best_combination)
    }
  )
)


