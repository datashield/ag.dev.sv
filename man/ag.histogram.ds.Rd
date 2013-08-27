\name{ag.histogram.ds}
\alias{ag.histogram.ds}
\title{Computes a histogram of the given data values without plotting.}
\usage{
  ag.histogram.ds(xvect, range)
}
\arguments{
  \item{xvect}{the numeric vector for which the histogram
  is desired.}

  \item{range}{a numeric vector, a minimum and a maximum.}
}
\value{
  a list with an object of class \code{histogram} and a
  vector of x-positions
}
\description{
  this functions produces the information required to plot
  a histogram. This is done without allowing for bins
  (cells) with a count of less than 5. If a bin has a count
  < 5 it is collapsed with the nearing bin; this process
  iterates until all bins have count >=5.
}
\examples{
\dontrun{
# load the file that contains the login details
library(dsbaseclient)
data(logindata)

opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a histogram object without plotting
hist.object <- datashield.aggregate(opals, quote(ag.histogram.ds(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}

