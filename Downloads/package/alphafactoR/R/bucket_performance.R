#' @title Creates an xts of the mean forward returns per factor bucket.
#'
#' @param factor_buckets An xts object containing the buckets for each asset.
#' @param forward_returns An xts object of na padded forward returns for each asset.
#' @param average_func A string of the  unction name used to average the forward returns.

bucket_performance <- function(factor_buckets,
                               forward_returns,
                               average_func = c('mean', 'median')){

  if(!identical(dim(factor_buckets), dim(forward_returns))){
    stop('factor_buckets and forward returns must have matching dimensions')
  }

  avg <- match.fun(average_func)
  bucket_sequence <- sort(unique(factor_buckets), decreasing = FALSE)
  # Create a list containing the average forward returns of each bucket, ordered by the row they exist in.
  bucket_returns <- lapply(bucket_sequence, function(i){

    sapply(1:nrow(factor_buckets), function(j){
      column_index <- which(factor_buckets[j] %in% i)
      selected_returns <- forward_returns[j, column_index]
      aggregated_returns <- avg(selected_returns)
    })

  })
  return_matrix <- Reduce('cbind', bucket_returns)
  return_xts <- xts(return_matrix, order.by = index(factor_buckets))
  colnames(return_xts) <- bucket_sequence
  return(return_xts)
}
## TODO: Make factor_quantiles function - must be in quantiles not ranks for correlation. If in ranks, highest/lowest changes with +/- columns
## TODO: Add reallocation_period
