#' @title Trims the maximum number of leading NAs from every matrix in a list of matrices.
#'
#' @param factor_list A list of matrices or xts objects of equal length.
#'
#' @return A list of matrices or xts objects of equal length without NAs.
#'
#' @note This functions ensures that all factors contribute to the factor score in factor_scores regardless of lookback period.
#' @note All other functions outside of factorapply depend on this structure.
#'
trim_max_lookback <- function(factor_list){

  # Count number of rows where all elements are NA
  na_count <- sapply(factor_list, function(i) {
    nrow(i[rowSums(is.na(i)) == ncol(i), ])
  })

  # Remove first n_rows from matrices
  trim_count <- max(na_count)
  trimmed_list <- lapply(factor_list, function(i){
    i <- i[-(1:trim_count),]
  })

  return(trimmed_list)
}
