\name{iv.str.all}
\alias{iv.str.all}
\title{Calculate Information Value for all character columns in data frame}
\usage{
  iv.str.all(df, y, summary = F)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{y}{column (integer) with binary outcome}

  \item{summary}{Only total information value for variable
  is returned when summary is TRUE}
}
\description{
  Calculates information value for all character vectors in
  given data frame
}
\examples{
iv.str.all(german_data,"gbbin")
iv.str.all(german_data,"gbbin",T)
}

