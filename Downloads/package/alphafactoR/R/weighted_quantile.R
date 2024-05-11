#' @title Buckets a list of equal length vectors into based on weighted quantiles
#'
#' @param x A list of equal length vectors.
#' @param weights A vector of weights equal to the length of the list, with each weight being applied to the respective index of the list.
#' @param probs A vector or list of probabilities for each quantile bucket.
#'
#' @references
#'  rpatel (https://stats.stackexchange.com/users/79997/rpatel). Combining Z Scores by Weighted Average.
#'  \url{https://stats.stackexchange.com/q/348605} (version: 2019-04-27).
#'
#'  @note This method calculates z scores, finds the weighted average, and then restandardizes those scores.
weighted_quantile <- function(x, weights, probs = seq(0, 1, .25)){
  # Multiplies the z score of each distribution within x by its weight, sums them,
  # and divides by the square root of the sum of squared weights to re-standardize.
  squared_weights <- sapply(weights, function(i){
    i^2
  })

  sqrtofssw <- sqrt(sum(squared_weights))

  weighted_zscores <- lapply(seq_along(x), function(i){
    # Change to numeric in order to deal with xts objects
    x[[i]] <- as.numeric(x[[i]])
    x[[i]] <- t(scale(x[[i]]))
    x[[i]] <- (x[[i]] * weights[i])
  })
  restandardized_zscores <- Reduce('+', weighted_zscores) / sqrtofssw
  # Remove attricutes
  attributes(restandardized_zscores) <- NULL
  quantiles <- pnorm(q = restandardized_zscores)
  bucketed_quantiles <- cut(quantiles, breaks = probs, labels = FALSE)
  return(bucketed_quantiles)
}
