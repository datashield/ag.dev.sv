\name{ag.quantilemean.ds}
\alias{ag.quantilemean.ds}
\title{Provides quantiles and mean information without maximum and minimum}
\usage{
  ag.quantilemean.ds(xvect)
}
\arguments{
  \item{xvect}{a numerical vector}
}
\value{
  a numeric vector that represents the sample quantiles
}
\description{
  Provides quantiles and mean information without maximum
  and minimum
}
\examples{
\dontrun{
# load the file that contains the login details
library(dsbaseclient)
data(logindata)

opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the sample quantiles that correspond to the probabilities specified internally
quantiles <- datashield.aggregate(opals, quote(ag.quantilemean.ds(D$LAB_TSC)))
}
}
\author{
  Burton, P.; Gaye, A.
}

