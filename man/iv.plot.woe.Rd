\name{iv.plot.woe}
\alias{iv.plot.woe}
\title{Plot WoE patterns for variables}
\usage{
  iv.plot.woe(iv)
}
\arguments{
  \item{iv}{WoE data for variables - output from iv.mult
  with summary=FALSE (false is default for iv.mult)}
}
\description{
  Plots calculated WoE for every variable
}
\examples{
iv.plot.woe(iv.mult(german_data,"gbbin"))
iv.plot.woe(iv.mult(german_data,"gbbin",vars=c("ca_status","housing","job","mob"),summary=FALSE))
}

