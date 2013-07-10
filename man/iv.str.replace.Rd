\name{iv.str.replace}
\alias{iv.str.replace}
\title{Replace values with infofrmation value}
\usage{
  iv.str.replace(df, iv)
}
\arguments{
  \item{df}{data frame with original data}

  \item{iv}{list of information values for variables -
  output from iv.str.mult}
}
\description{
  Calculates information value for all character vectors in
  given data frame
}
\examples{
outiv <- iv.str.mult(german_data,"gbbin",vars=c("ca_status","housing","job"))
x <- iv.str.replace(german_data,outiv)
str(x)
outiv <- iv.str.mult(german_data,"gbbin")
x <- iv.str.replace(german_data,outiv)
str(x)
}

