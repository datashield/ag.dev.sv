\name{ag.NROW.ds}
\alias{ag.NROW.ds}
\title{Returns the number of rows or columns present in a 1-column matrix vector}
\usage{
  ag.NROW.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector}
}
\value{
  a numeric
}
\description{
  Returns the number of rows or columns present in a
  1-column matrix vector
}
\details{
  The number of rows is returned only if the vector contain
  more than 4 entries.
}
\examples{
\dontrun{
# load the file that contains the login details
library(dsbaseclient)
data(logindata)

opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# run the function
stat.mean <- datashield.aggregate(opals, quote(NROW(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

