
#' Greedy Knapsack class
#'
#'
#'In greedy approach to knapsack problem.The unit value per weight is calculated for each item.
#'After calculating unit per weight, the items with maximum values are selected in order.
#'Selected items total must be below knapsack weight \code{W}.
#'This approach suppose not to give an exact value but return at least 50% of the maximum value.
#'
#'
#' @export
#' @exportClass greedy_knapsack
#' @import methods
#' @importFrom utils combn

greedy_knapsack <- setRefClass(
                'greedy_knapsack',
                  methods = list(
                    greedyKnapSack = function(df, W){
                      if (
                        length(colnames(df))!=2 ||
                        !is.numeric(W) ||
                        !all(colnames(df)==c("w","v")) ||
                        !is.data.frame(df) ||
                        !all(df[,'w']>0) ||
                        !all(df[,'w']>0) ||
                        W <0
                        ){
                        stop("Please provide a correct input")
                      }
                      w = df[,'w']
                      v=df[,'v']

                      ratio <-v/w
                      ratio[order(ratio,decreasing=TRUE)]
                      factors <-order(ratio,decreasing=TRUE)

                      #putting into bag with maximum value
                      greedyBag<-c()
                      j=1
                      while (sum(greedyBag) <= W)
                      {
                        greedyBag <- c(greedyBag,w[factors[j]])
                        j <- j+1
                      }
                      result <- list(value = round(sum(v[factors[1:(j-2)]]), digits = 0), factors = factors[1:(j-2)])

                       return(result)
                    }
                ))


