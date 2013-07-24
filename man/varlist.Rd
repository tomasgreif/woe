\name{varlist}
\alias{varlist}
\title{Subset variables in data frame}
\usage{
  varlist(df = NULL,
    type = c("numeric", "factor", "character"),
    pattern = "", exclude = NULL)
}
\arguments{
  \item{iv}{Information values summary for variables -
  output from iv.mult with summary=T}

  \item{type}{One of "numeric", "factor", "character" or
  any combination as vector}

  \item{pattern}{regexp pattern used to filter variables}

  \item{exclude}{list of variables to be excluded}
}
\description{
  Get subset of variables in data frame based on type,
  regexp pattern and specified exclusions.
}
\examples{
# All variable starting with cred
varlist(german_data,pattern="^cred")
# All numeric variable
varlist(german_data,type="numeric")
# All factor variable except variable gb and variables starting with c
varlist(german_data,type="factor",exclude=c("gb",varlist(german_data,pattern="^c")))
# Same as previous, only using pattern instead of c()
varlist(german_data,type="factor",exclude=varlist(german_data,pattern="^c|gb"))
# Everything but factors
varlist(german_data,exclude=varlist(german_data,"factor"))
# Use sapply to apply function for variables
sapply(german_data[,varlist(german_data,type="numeric",pattern="credit")], summary)
sapply(german_data[,varlist(german_data,type="numeric",pattern="credit")], function (x) length(unique(x)))
}

