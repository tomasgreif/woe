\name{iv.replace.woe}
\alias{iv.replace.woe}
\title{Replace values with information value}
\usage{
  iv.replace.woe(df, iv, verbose = FALSE)
}
\arguments{
  \item{df}{data frame with original data}

  \item{iv}{list of information values for variables -
  output from iv.str.mult}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  Calculates information value for all character vectors in
  given data frame
}
\examples{
outiv <- iv.mult(german_data,"gbbin",vars=c("ca_status","housing","mob"))
x <- iv.replace.woe(german_data,outiv)
str(x)
outiv <- iv.mult(german_data,"gbbin")
x <- iv.replace.woe(german_data,outiv)
str(x)
}

