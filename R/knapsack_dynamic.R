

#' Dynamic Knapsack
#'
#' This is a dynamic programming (DP) approach to solving knapsack problem.
#' Here a tabulation method is used. DP approach results into better performance
#'
#' @param x Data frame weights and values
#' @param W Total weight of the knapsack
#'
#' @return
#' maximum obtained value
#'
#' @export
#' @exportClass knapsack_dynamic
#'
#' @importFrom utils combn


knapsack_dynamic <- setRefClass(
    'knapsack_dynamic',
      methods = list(
        knapsackDynamic = function(df,W){
          if (length(colnames(df))!=2 || !is.numeric(W) || !all(colnames(df)==c("w","v")) || !is.data.frame(df) || !all(df[,'w']>0) || !all(df[,'w']> 0) || W < 0 ) stop("Wrong input")

          v = df[,'v']
          w = df[,'w']
          n=length(w)

          #create empty matrix of zeros
          mz<-matrix(0,n+1,W+1)
          for (i in 2:(n+1)){
            for (c in 2:(W+1)){
              if (w[i-1]>c)
                mz[i,c]=mz[i-1,c]
              else
                mz[i,c]=max( v[i-1]+mz[i-1,c-w[i-1]], mz[i-1,c] )
            }
          }

          #identify selected items
          factors<-c()
          nW <- W+1
          for (j in (n+1):2){
            if (mz[j,nW] != mz[j-1,nW]){
              factors <- c(factors,j-1)
              nW <- nW - w[j-1]

            }
          }
          result <- list(value=round(mz[n+1,W+1],digits=0),factors=factors[order(factors)])
          return(result )

        }
      )
    )




