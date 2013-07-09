\name{iv.str}
\alias{iv.str}
\title{Calculate Information Value for character vectors}
\usage{
  iv.str(df, x, y)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{x}{column (character) for which Information Value
  should be calculated}

  \item{y}{column (integer) with binary outcome}
}
\description{
  This function calculates Information Value for character
  vectors
}
\examples{
iv.str(german_data,"purpose","gbbin")
}

