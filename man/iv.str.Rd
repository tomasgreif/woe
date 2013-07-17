\name{iv.str}
\alias{iv.str}
\title{Calculate Information Value for character or factor columns}
\usage{
  iv.str(df, x, y, verbose = FALSE)
}
\arguments{
  \item{df}{data frame with at least two columns (predictor
  x and outcome y)}

  \item{x}{column (character or factor) for which
  Information Value should be calculated}

  \item{y}{column (integer) with binary outcome. y has to
  be a column in df data frame. y has to be numeric
  (integer or double) with values 0 and 1. It is
  recommended that 1 is used for predicted event and 0
  otherwise.}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  This function calculates information value for character
  or factor columns of data frame.
}
\examples{
iv.str(german_data,"purpose","gbbin")
iv.str(german_data,"savings","gbbin")
}

