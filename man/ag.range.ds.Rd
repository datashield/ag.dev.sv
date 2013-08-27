\name{ag.range.ds}
\alias{ag.range.ds}
\title{returns the minimum and maximum of a numeric vector}
\usage{
  ag.range.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical}
}
\value{
  a numeric vector which contains the minimum and the
  maximum
}
\description{
  this function is similar to R function \code{range} but
  instead to not return the real minimum and maximum, the
  computed values are multiplied by a very small random
  number.
}
\examples{
\dontrun{
# load the login data
library(dsbaseclient)
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_HDL")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# check if the variable 'LAB_HDL' is empty
datashield.aggregate(opals, quote(range.ds(D$LAB_HDL)))
}
}
\author{
  Gaye, A.
}
