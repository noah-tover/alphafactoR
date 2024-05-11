#' @title Places factor data into discrete groups.
#'
#' @param factor_data An xts object
#' @param method balanced allows for buckets of relatively equal content, most likely with some remainder if width is not a multiple of the length #'  of a row. proximal allows for bucketing based on the proximity to other values. fixed is the method that must be selected to use predefined  #'  intervals for bucketing
#' @param width The number of buckets. If less than one, treated as a percentage of the non NA values in any row.
#' @param intervals A vector of numbers representing the intervals for each bucket.
#'
#' @note This function is useful for bounding ranks into set intervals, or scoring bounded measures like a correlation coefficient.

factor_bucket <- function(factor_data,
                          method = c('balanced', 'proximal', 'fixed'),
                          width,
                          intervals){

  # Simplify data for compatibility with cut function
  simplified_data <- drop(coredata(factor_data))

  if(method == 'balanced' | method == 'proximal'){
    bucketed_data <- apply(simplified_data, MARGIN = 1, function(i){

      if(0 < width & width < 1){
        width <- length(na.omit(i)) * width
      }

      if(method == 'balanced'){
        # Make data equally spaced - this allows cut to make balanced buckets
        i <- rank(as.numeric(i), na.last = 'keep')
      }

      #TODO: Why is as.numeric necessary?
      cut(i, breaks = width, labels = FALSE)
    })
  }

  if(method == 'fixed'){
    bucketed_data <- apply(simplified_data, MARGIN = 1, cut, breaks = intervals, labels = FALSE)
  }

  bucketed_xts <- xts::xts(t(bucketed_data), order.by = index(factor_data))
  colnames(bucketed_xts) <- colnames(factor_data)
  return(bucketed_xts)
}

##TODO: Add argument for specifying which bucket the remainder(s) will be placed in.
