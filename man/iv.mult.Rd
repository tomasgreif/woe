\name{iv.mult}
\alias{iv.mult}
\title{Calculate Information Value for defined columns in data frame}
\usage{
  iv.mult(df, y, summary = FALSE, vars = NULL,
    verbose = FALSE, rcontrol = NULL)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{y}{column (integer) with binary outcome}

  \item{summary}{Only total information value for variable
  is returned when summary is TRUE. Output is sorted by
  information value, starting with highest value.}

  \item{vars}{List of variables. If not specified, all
  character variables will be used}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  Calculates information value for defined columns in given
  data frame. Columns can have numeric or character type
  (including factors).
}
\details{
  Information Value (IV) is concept used in risk management
  to assess predictive power of variable. IV is defined as:
  WoE (Weight of Evidence) is defined as:
}
\examples{
iv.mult(german_data,"gb")
iv.mult(german_data,"gb",TRUE)
iv.mult(german_data,"gb",TRUE,c("ca_status","housing","job","duration")) # str(german_data)
iv.mult(german_data,"gb",vars=c("ca_status","housing","job","duration"))
iv.mult(german_data,"gb",summary=TRUE, verbose=TRUE)
iv.mult(german_data,"gb",vars=c("duration","age"),rcontrol=rpart.control(cp=.001,minbucket=10))
iv.mult(german_data,"gb",summary=TRUE,rcontrol=rpart.control(cp=.001,minbucket=50))
}

