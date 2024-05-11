#' @title Nonparametric method for estimating factor scores.
#'
#' @param factor_list A list of equal length matrices or xts objects of factors
#' @param weights: A list or vector with the same length as the number of columns #' in any xts or matrix object within factor_list. This list or #'  vector should sum #' to one. Alternatively, an xts object can be specified, in which case the #'dimensions of weights must match those of any #'  matrix or xts within factor_list. #'Each row of the weights must sum to one. The list or column index of weights #'corresponds to the index of #'  the factor_list to which the weights refer.
#' @param format The date format of the xts object, used to turn matrix back into #' xts
#' @param combine_function A string containing the name of the function used to combine weighted scores, if weights is specified. Defaults to sum scores.
#'
#' @return Returns a list of ranked matrix or xts objects if weights is not #'specified.
#' @return If weights is specified, returns a single matrix or xts containing a #'weighted sum of factor scores.
#'
#' @references
#'  DiStefano, C., Zhu, M., & Mîndrilã, D. (2009). Understanding and Using Factor Scores:
#'  Considerations for the Applied Researcher. *Practical Assessment, Research, and Evaluation*, 14(20).


# TODO: The weights method for ties should have some option to classify any rank #falling within some interval a tie. Ex: 6.1 and 6 considered a tie as +-n
factor_rank <- function(factor_list,
                        weights,
                        format = "%Y-%m-%d",
                        ties.method = c('average', 'first', 'last', 'random', 'max', 'min'),
                        combine_function = '+'){

  # Initialize loop to rank each
  ranked_factors <- lapply(X = factor_list, FUN = function(i){
    # Rank each column within each matrix by row. Only rank non NA values.
    ranked_matrix <- t(apply(X = i, MARGIN = 1, FUN = rank, na.last = 'keep', ties.method = ties.method))
    dates <- as.Date(rownames(i), format = format)
    ranked_xts <- xts::xts(ranked_matrix, order.by=index(i))
  })

  if(missing(weights)){
    return(ranked_factors)
  }

  else if(is.list(weights) || is.vector(weights)) {

    # Multiply each matrix of ranking by their weights
    weighted_factors <- lapply(seq_along(ranked_factors), function(i) {
      ranked_factors[[i]] * weights[i]
    })

    factor_scores <- Reduce(combine_function, weighted_factors)
  }

  else if(is.matrix(weights)){

    weighted_factors <- lapply(seq_along(ranked_factors), function(i){
      sweep(ranked_factors[[i]], 1, weights[, i], `*`)
    })

    factor_scores <- Reduce(combine_function, weighted_factors)
  }

  return(factor_scores)
}
