##TODO: Add function to detect structure of end object of matrix and recreate xts to hide information better.
##TODO: Further efficiency optimization
##TODO: Parallel support
##########################################################
#' @title Creates a list of matrices or time series resulting from a list of different functions.
#'
#' @param price A numeric matrix or xts object.
#' @param factors A list containing strings corresponding to function names to call.
#' @param arguments A list of lists containing corresponding arguments to each function. If no arguments are specified, an empty list must exist for that function.
#' @param lag Specifies whether or not to lag all columns by one. Defaults to true.
#'
#' @return Returns a list of xts resulting from the functions specified in the factors list.
## TODO: Use tryCatch to allow user to input correct arguments given arguments are invalid for ith

factorapply <- function(price,
                        functions,
                        arguments,
                        lag = FALSE){

  # Check variables
  if(!xts::is.xts(price)){
    stop("The price parameter must be an xts object")
  }

  if(!is.list(functions) || !all(sapply(functions, is.character))){
    stop("The functions parameter must be a list containing function names as strings")
  }

  if(length(arguments) != length(functions)){
    stop("Each function must have a corresponding list of arguments within the master arguments list. if no arguments are specified, an empty list must be within arguments for that function.")
  }

  # Loop over each factor and its respective argument
  list <- lapply(1:length(functions), function(i) {
    ith_factor <- functions[[i]]
    ith_arguments <- arguments[[i]]
    # Loop over each column, applying the  ith function
    columns <- apply(MARGIN = 2, X = price, FUN = function(j) {
      args_list <- c(list(j), ith_arguments)
      result_column <- do.call(ith_factor, args_list)
      return(result_column)
    })

    # Add function name to column name
    newnames <- paste0(colnames(price), "_", ith_factor)
    colnames(columns) <- newnames

    if(lag == TRUE){
      columns <- stats::lag(columns, 1)
    }

    # Reconstruct xts object
    index <- index(price)
    columns <- xts(columns, order.by = index)
  })

  return(list)
}
