\name{iv.str.replace.woe}
\alias{iv.str.replace.woe}
\title{Replace values with information value}
\usage{
  iv.str.replace.woe(df, iv)
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
x <- iv.str.replace.woe(german_data,outiv)
str(x)
outiv <- iv.mult(german_data,"gbbin")
x <- iv.str.replace.woe(german_data,outiv)
str(x)
}

