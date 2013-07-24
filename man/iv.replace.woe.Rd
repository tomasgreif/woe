\name{iv.replace.woe}
\alias{iv.replace.woe}
\title{Replace raw variables with Weight of Evidence}
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
  Replaces variables in data frame with Weight of Evidence.
  This will add new columns with "_woe" suffix to specified
  data frame.
}
\examples{
outiv <- iv.mult(german_data,"gb",vars=c("ca_status","housing","duration"))
x <- iv.replace.woe(german_data,outiv)
str(x)
outiv <- iv.mult(german_data,"gb")
x <- iv.replace.woe(german_data,outiv)
str(x)
}

