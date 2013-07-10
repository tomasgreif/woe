\name{iv.str.mult}
\alias{iv.str.mult}
\title{Calculate Information Value for all character columns in data frame}
\usage{
  iv.str.mult(df, y, summary = F, vars = NULL)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{y}{column (integer) with binary outcome}

  \item{summary}{Only total information value for variable
  is returned when summary is TRUE. Output is sorted by
  information value, starting with highest value.}

  \item{vars}{List of variables. If not specified, all
  character variables will be used}
}
\description{
  Calculates information value for all character vectors in
  given data frame
}
\examples{
iv.str.mult(german_data,"gbbin")
iv.str.mult(german_data,"gbbin",T)
iv.str.mult(german_data,"gbbin",T,c("ca_status","housing","job"))
iv.str.mult(german_data,"gbbin",vars=c("ca_status","housing","job"))
}

