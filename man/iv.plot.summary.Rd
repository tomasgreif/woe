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
iv.plot.summary(iv.mult(german_data,"gb",TRUE))
iv.plot.summary(iv.mult(german_data,"gb",vars=c("ca_status","housing","job","duration"),summary=TRUE))
}

