\name{ag.checkvar1.ds}
\alias{ag.checkvar1.ds}
\title{Checks that variables do exist and do not contains missing data only.}
\usage{
  ag.checkvar1.ds(dataset, variable)
}
\arguments{
  \item{dataset}{a \code{dataframe} that contains the data
  assigned to R}

  \item{variable}{the variables to check}
}
\value{
  '0' if the variable did not fail any of the checks and
  '1' if the variable failed one or more checks.
}
\description{
  this functions checks if the variables are present in the
  dataset assigned to R from an opal datasource and if the
  variables are empty (contain NAs only). This function is
  internal (not available to users) as it is only required
  by \code{ds.glm}.
}
\author{
  Gaye, A.
}

