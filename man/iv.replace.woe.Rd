\name{iv.replace.woe}
\alias{iv.replace.woe}
\title{Replace raw variables with Weight of Evidence}
\usage{
  iv.replace.woe(df, iv, verbose = FALSE)
}
\arguments{
  \item{df}{data frame with original data}

  \item{iv}{list of information values for variables -
  output from \code{\link{iv.mult}} with
  \code{summary=FALSE}.}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  Replaces variables in data frame with Weight of Evidence.
  This will add new columns with "_woe" suffix to specified
  data frame.
}
\examples{
# Replace WoE for list of variables
outiv <- iv.mult(german_data,"gb",vars=c("ca_status","housing","duration"))
x <- iv.replace.woe(german_data,outiv)
str(x)

# Replace WoE for all variables
outiv <- iv.mult(german_data,"gb")
x <- iv.replace.woe(german_data,outiv)
str(x)

# Replace WoE for all numeric variables- ultimate one-liner
x <- iv.replace.woe(german_data,iv.mult(german_data,"gb",vars=varlist(german_data,"numeric")))
str(x)
}

