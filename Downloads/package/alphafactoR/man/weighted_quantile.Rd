% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_quantile.R
\name{weighted_quantile}
\alias{weighted_quantile}
\title{Buckets a list of equal length vectors into based on weighted quantiles}
\usage{
weighted_quantile(x, weights, probs = seq(0, 1, 0.25))
}
\arguments{
\item{x}{A list of equal length vectors.}

\item{weights}{A vector of weights equal to the length of the list, with each weight being applied to the respective index of the list.}

\item{probs}{A vector or list of probabilities for each quantile bucket.}
}
\description{
Buckets a list of equal length vectors into based on weighted quantiles
}
\references{
rpatel (https://stats.stackexchange.com/users/79997/rpatel). Combining Z Scores by Weighted Average.
\url{https://stats.stackexchange.com/q/348605} (version: 2019-04-27).

@note This method calculates z scores, finds the weighted average, and then restandardizes those scores.
}
