\name{iv.binning.simple}
\alias{iv.binning.simple}
\title{Simple binning of numeric variable}
\usage{
  iv.binning.simple(df, vars, by = 0.1, eout = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{df}{data frame with at least one numeric column}

  \item{vars}{numeric column(x) to be binned}

  \item{by}{parameter for quantile based binning. Default
  value 0.1 will create 10 bins}

  \item{eout}{Exclude outliers. If TRUE (default) then
  outliers as defined in boxplot are excluded before
  binning and included afterwards as separated bins.}

  \item{verbose}{Prints additional details when TRUE.
  Useful mainly for debugging.}
}
\description{
  This functions bins numeric variable into bins with
  unique values.
}
\examples{
iv.binning.simple(german_data,"credit_amount")
iv.binning.simple(german_data,"credit_amount",by=0.05)
iv.binning.simple(german_data,"credit_amount",eout=FALSE)
iv.binning.simple(german_data,"age",by=0.2)
german_data$cut_cra1 <- iv.binning.simple(german_data,"credit_amount",eout=TRUE)
german_data$cut_cra2 <- iv.binning.simple(german_data,"credit_amount",eout=FALSE)
iv.plot.woe(iv=iv.mult(german_data,"gb",vars=c("cut_cra1","cut_cra2")))
}

