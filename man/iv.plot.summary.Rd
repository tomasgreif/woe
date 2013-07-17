\name{iv.plot.summary}
\alias{iv.plot.summary}
\title{Plot summay information values for variables}
\usage{
  iv.plot.summary(iv)
}
\arguments{
  \item{iv}{Information values summary for variables -
  output from iv.mult with summary=T}
}
\description{
  Plots calculated information values
}
\examples{
iv.plot.summary(iv.mult(german_data,"gbbin",TRUE))
iv.plot.summary(iv.mult(german_data,"gbbin",vars=c("ca_status","housing","job","mob"),summary=TRUE))
}

