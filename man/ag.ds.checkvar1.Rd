\name{ag.ds.checkvar1}
\alias{ag.ds.checkvar1}
\title{Checks that variables do exist and do not contains missing data only.}
\usage{
  ag.ds.checkvar1(data, servername, variables)
}
\arguments{
  \item{data}{a \code{dataframe} that contains the data
  assigned to R}

  \item{servername}{name of the login object the data were
  assigned to}

  \item{variables}{the variables to check}
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

