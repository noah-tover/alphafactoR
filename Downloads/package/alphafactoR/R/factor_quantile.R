#' @title Buckets factors into quantiles
#'
#' @param factor_data An xts object or a list of xts objects
#' @param probs A list or vector of the probabilities of each quantile.
#'
#' @param weights A vector containing weights to apply to each xts within the list.
#' @param method Which quantizing method to choose. Weighted uses weighted z scores to find the quantiles.
factor_quantile <- function(factor_data, method = c('traditional', 'weighted'), probs = seq(0, 1, .25), weights){
  if(method == 'traditional'){
    quantized_data <- apply(drop(coredata(factor_data)), MARGIN = 1, function(i){
      ith_quantile <- quantile(i, probs = probs, na.rm = TRUE, names = FALSE)
      # Change endpoints by arbitrary value to include all values in cut intervals
      ith_quantile[1] <- ith_quantile[1] - (ith_quantile[1]/1000)
      as.numeric(cut(i, breaks = ith_quantile))
    })
  }
  if(method == 'weighted'){
    # weighted_quantile expects x to be a list of vectors, to meet this dependency must operate on each row within the list of xts objects-
    # simultaneously
    split_list <- lapply(factor_data, FUN = function(i){
      i <- drop(coredata(i)) # Speeds up split
      split(i, 1:NROW(i))
    })

    list_of_rows <- Reduce('cbind', split_list)
    # list 1, list 2, list n
    # row1    row1    row1
    # row2    row2    row2
    # ...
    quantized_data <- (apply(list_of_rows,
                             MARGIN = 1,
                             FUN = weighted_quantile,
                             weights = weights))
  }
  if(is.list(factor_data)){
    original_xts <- factor_data[[1]]
  }
  else {
    original_xts <- factor_data
  }
  quantized_xts <- xts::xts(t(quantized_data), order.by = index(original_xts))
  colnames(quantized_xts) <- colnames(original_xts)
  return(quantized_xts)
}
