\name{iv.str}
\alias{iv.str}
\title{Calculate Information Value for character vectors}
\usage{
  iv.str(df, x, y, na.rm = T, exclusion)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{x}{column (character) for which Information Value
  should be calculated}

  \item{y}{column (integer) with binary outcome}

  \item{na.rm}{Defines how missing values are handled}

  \item{exclusion}{Vector of values to be excluded}
}
\description{
  This function calculates Information Value for character
  vectors
}
\examples{
iv.str(german_data,purpose,gb)
arrange(mtcars, cyl, disp)
arrange(mtcars, cyl, desc(disp))
}
\keyword{manip}

