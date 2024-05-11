data(assets)
specdata <- assets

test_that('Each xts object within the list is the result of a specified function', {
  diff_n <- 12
  cumsum_n <- 4
  diff_dataset <- apply(specdata, MARGIN = 2, FUN = diff, n = diff_n)
  cumsum_dataset <- apply(specdata, MARGIN = 2, FUN = cumsum, n = cumsum_n)
  diff_dataset_xts <- xts(diff_dataset, order.by = index(specdata))
  cumsum_dataset_xts <- xts(cumsum_dataset, order.by = index(specdata))
  # Add column names identical to factorapply
  colnames(diff_dataset_xts) <- paste0(colnames(specdata), "_", 'diff')
  colnames(cumsum_dataset_xts) <- paste0(colnames(specdata), "_", 'cumsum')
  expected <- list(diff_dataset_xts, cumsum_dataset_xts)
  # Tested list
  funcs <- list('diff', 'cumsum')
  args <- list(list(n = diff_n), list(n = cumsum_n))
  tested <- factorapply(price = specdata, functions = funcs, arguments = args, lag = FALSE)
  expect_identical(object = tested, expected = expected)
})


