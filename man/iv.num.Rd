\name{iv.num}
\alias{iv.num}
\title{Calculate Information Value for numeric (double/integer) vectors}
\usage{
  iv.num(df, x, y, verbose = FALSE)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{x}{column (character) for which Information Value
  should be calculated}

  \item{y}{column (integer/factor) with binary outcome}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  This function calculates information value for numeric
  vectors. This is done using decision tree.
}
\examples{
iv.num(german_data,"duration","gb")
iv.num(german_data,"age","gb")
}

