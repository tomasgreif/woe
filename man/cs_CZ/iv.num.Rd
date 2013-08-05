\name{iv.num}
\alias{iv.num}
\title{Testovací česká nápověda pro iv.num}
\usage{
  iv.num(df, x, y, verbose = FALSE, rcontrol = NULL)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{x}{column (numeric) for which Information Value
  should be calculated}

  \item{y}{column (integer/factor) with binary outcome.  It
  is suggested that y is factor with two levels "bad" and
  "good" If there are no levels good/bad than the following
  assumptions are applied - if y is integer, than 0=good
  and 1=bad. If y is factor than level 2 is assumed to mean
  bad and 1 good.}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}

  \item{rcontrol}{Additional parameters used for rpart tree
  generation. Use \code{?rpart.control()} to get more
  details.}
}
\description{
  This function calculates information value for numeric
  vectors. This is done using decision tree.
}
\examples{
iv.num(german_data,"duration","gb")
iv.num(german_data,"age","gb")
iv.num(german_data,"age","gb")
iv.num(german_data,"duration","gb",rcontrol=rpart.control(cp=.01,minbucket=100))
}

