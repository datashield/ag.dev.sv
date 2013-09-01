\name{ag.mean.ds}
\alias{ag.mean.ds}
\title{Calculates a statistical mean for a vector with more than 4 entries}
\usage{
  ag.mean.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector}
}
\value{
  a numerical
}
\description{
  Calculates a statistical mean for a vector with more than
  4 entries
}
\examples{
\dontrun{
# load the file that contains the login details
data(logindata)

opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the statistical mean
stat.mean <- datashield.aggregate(opals, quote(ag.mean.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

