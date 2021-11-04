#' Brute Force Knapsack
#'
#'This algorithm estimates how to put available items into the knapsack
#'in order to get the maximum possible value less than weight of knapsack.
#'Here brute force approach is implemented that means all possible
#' combinations of items O(2^n) are considered.
#'
#' @param x Data frame with two numeric columns: weights and values
#' @param W Total weight of the knapsack
#' @param parallel logical parameter for parallel computation
#'
#' @return
#' List with numbers of items that can be put into knapsack and maximum obtained value
#
#' @export
#'
#'
#'
#'
brute_force_knapsack <- function(x,W, parallel = FALSE){

  if(!is.data.frame(x) || W < 0){
    stop("Wrong Input")
  }
  if(parallel == FALSE){
    list_comb <- list()
    for(j in 1:nrow(x))
    {
      list_comb[[j]] <- combn(rownames(x), j, paste, collapse = " ")
    }

    list_wght <- list()
    for(j in 1:nrow(x))
    {
      list_wght[[j]] <- combn(x$w, j,sum)
    }

    list_val <- list()
    for(j in 1:nrow(x) )
    {
      list_val[[j]] <- combn(x$v, j, sum)
    }

    vector_comb <- unlist(list_comb)
    vector_wght <- unlist(list_wght)
    vector_val <- round(unlist(list_val),0)

    weights_ind_cap <- which(vector_wght < W)
    value_vald <- vector_val[weights_ind_cap]
    maxvalid <- max(value_vald)

    valid_ind_vec <- which(vector_val == maxvalid)
    val_cmb <- vector_comb[valid_ind_vec]

    lead_comb_list <- list(value = maxvalid, elements = as.numeric(strsplit(val_cmb, " ")[[1]]))
  }else{

    no_of_cores <- detectCores() - 1
    cluster <- makeCluster(no_of_cores)
    clusterExport(cluster , c("x") ,envir = environment())

    list_comb <- parLapplyLB(cluster, 1:nrow(x), fun =  function(y) {
      combn(rownames(x) , y , paste0, collapse = " ")
    })
    list_wght <- parLapplyLB(cluster, 1:nrow(x), fun =  function(y) {
      combn(x$w , y, sum)
    })
    list_val <- parLapplyLB(cluster,1:nrow(x), fun =  function(y) {
      combn(x$v , y , sum)
    })

    stopCluster(cluster)

    vector_comb <- unlist(list_comb)
    vector_wght <- unlist(list_wght)
    vector_val <- round(unlist(list_val),0)

    weights_ind_cap <- which(vector_wght < W)
    value_vald <- vector_val[weights_ind_cap]
    maxvalid <- max(value_vald)

    valid_ind_vec <- which(vector_val == maxvalid)
    val_cmb <- vector_comb[valid_ind_vec]

    lead_comb_list <- list(value = maxvalid, elements = as.numeric(strsplit(val_cmb, " ")[[1]]))
  }
  return(lead_comb_list)
}
