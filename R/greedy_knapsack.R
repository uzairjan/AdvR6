
#' Greedy Knapsack class
#'
#' @param x an object of class data.frame with two variables v (values) and w (weights).
#' @param W numeric scalar object that represents the knapsack size.
#'
#' In greedy approach to knapsack problem.The unit value per weight is calculated for each item.
#' After calculating unit per weight, the items with maximum values are selected in order.
#' Selected items total must be below knapsack weight \code{W}.
#' This approach suppose not to give an exact value but return at least 50% of the maximum value.
#'
#'
#' @export greedy_knapsack
#' @import methods
#' @importFrom utils combn

greedy_knapsack = function(x, W) {
      if (
        length(colnames(x)) != 2 ||
          !is.numeric(W) ||
          !all(colnames(x) == c("w", "v")) ||
          !is.data.frame(x) ||
          !all(x[, "w"] > 0) ||
          !all(x[, "w"] > 0) ||
          W < 0
      ) {
        stop("Please provide a correct input")
      }
      ratio <- x[, "v"] / x[, "w"]
      ratio[order(ratio, decreasing = TRUE)]
      elements <- order(ratio, decreasing = TRUE)
      # putting into bag with maximum value
      greedyBag <- c()
      j <- 1
      while (sum(greedyBag) <= W) {
        greedyBag <- c(greedyBag, x[, "w"][elements[j]])
        j <- j + 1
      }
      result <- list(value = round(sum(x[, "v"][elements[1:(j - 2)]]), digits = 0), elements = elements[1:(j - 2)])
      return(result)
}

# greedy_knapsack(x= knapsack_objects[1:1200,],W=2000)
