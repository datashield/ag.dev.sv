\name{ag.checkvar.ds}
\alias{ag.checkvar.ds}
\title{Checks that variables do exist and do not contains missing data only.}
\usage{
  ag.checkvar.ds(dataset, variable)
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
\examples{
\dontrun{
# load the file that contains the login details
library(ag.dev.cl)
data(logindata)

opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# check that the variable 'LAB_TSC' is available and is not empty
var <- datashield.aggregate(opals, quote(ag.checkvar.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

