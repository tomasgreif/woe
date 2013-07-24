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
  be a column in df data frame. It is suggested that y is
  factor with two levels "bad" and "good" If there are no
  levels good/bad than the following assumptions are
  applied - if y is integer, than 0=good and 1=bad. If y is
  factor than level 2 is assumed to mean bad and 1 good.}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  This function calculates information value for character
  or factor columns of data frame.
}
\examples{
iv.str(german_data,"purpose","gb")
iv.str(german_data,"savings","gb")
}

